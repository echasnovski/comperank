#' Specific functions for computing Head-to-Head matrices
#'
#' Specific functions for computing Head-to-Head matrices.
#'
#' @param cr_data Competition results in format ready for
#'   \code{\link[=results-longcr]{to_longcr}}.
#' @param players Vector of players for which Head-to-Head is computed.
#' @param absent_players Function which performs actions on Head-to-Head matrix
#'   dealing with players which absent in \code{cr_data}.
#' @param absent_h2h Function which performs actions on Head-to-Head matrix
#'   dealing with absent Head-to-Head records for some pairs of players.
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
#' All \code{get_h2h_*} are wrappers around \code{\link[=head-to-head]{get_h2h}}
#' for using a specific \code{h2h_fun}.
#'
#' @return \code{get_h2h_*} return \link[=head-to-head]{Head-to-Head matrices}.
#'
#' All \code{h2h_*} are implementations of \code{h2h_fun} and return a single
#' Head-to-Head value.
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
#' get_h2h_num_wins(cr_data)
#' get_h2h_num_wins(cr_data, half_for_draw = TRUE)
#' get_h2h_num_wins(cr_data, half_for_draw = TRUE, fill = 0)
#'
#' # Computing Head-to-Head values based on matchups
#' h2h_mean_score_diff(matchup_data[2, ])
#' h2h_mean_score_diff(matchup_data[19, ])
#'
#' @seealso \link{head-to-head} Abstract computing of Head-to-Head matrices.
#' @seealso \link{head-to-head-helpers} Head-to-Head helpers.
#' @name head-to-head-functions
NULL


# h2h_mean_score_diff ---------------------------------------------------------
#' @rdname head-to-head-functions
#' @export
get_h2h_mean_score_diff <-
  function(cr_data, players = NULL, absent_players = players_drop,
           absent_h2h = fill_h2h, ...) {
    get_h2h(
      cr_data = cr_data, h2h_fun = h2h_mean_score_diff,
      players = players,
      absent_players = absent_players, absent_h2h = absent_h2h,
      ...
    )
  }

#' @rdname head-to-head-functions
#' @export
h2h_mean_score_diff <- function(matchup_data, ...) {
  mean(matchup_data$score2 - matchup_data$score1)
}


# h2h_mean_score --------------------------------------------------------------
#' @rdname head-to-head-functions
#' @export
get_h2h_mean_score <-
  function(cr_data, players = NULL, absent_players = players_drop,
           absent_h2h = fill_h2h, ...) {
    get_h2h(
      cr_data = cr_data, h2h_fun = h2h_mean_score,
      players = players,
      absent_players = absent_players, absent_h2h = absent_h2h,
      ...
    )
  }

#' @rdname head-to-head-functions
#' @export
h2h_mean_score <- function(matchup_data, ...) {
  mean(matchup_data$score2)
}

# h2h_sum_score_diff ----------------------------------------------------------
#' @rdname head-to-head-functions
#' @export
get_h2h_sum_score_diff <-
  function(cr_data, players = NULL, absent_players = players_drop,
           absent_h2h = fill_h2h, ...) {
    get_h2h(
      cr_data = cr_data, h2h_fun = h2h_sum_score_diff,
      players = players,
      absent_players = absent_players, absent_h2h = absent_h2h,
      ...
    )
  }

#' @rdname head-to-head-functions
#' @export
h2h_sum_score_diff <- function(matchup_data, ...) {
  sum(matchup_data$score2 - matchup_data$score1)
}



# h2h_sum_score ---------------------------------------------------------------
#' @rdname head-to-head-functions
#' @export
get_h2h_sum_score <-
  function(cr_data, players = NULL, absent_players = players_drop,
           absent_h2h = fill_h2h, ...) {
    get_h2h(
      cr_data = cr_data, h2h_fun = h2h_sum_score,
      players = players,
      absent_players = absent_players, absent_h2h = absent_h2h,
      ...
    )
  }

#' @rdname head-to-head-functions
#' @export
h2h_sum_score <- function(matchup_data, ...) {
  sum(matchup_data$score2)
}
# h2h_num_wins ----------------------------------------------------------------
#' @rdname head-to-head-functions
#' @export
get_h2h_num_wins <-
  function(cr_data, players = NULL, absent_players = players_drop,
           absent_h2h = fill_h2h, ...) {
    get_h2h(
      cr_data = cr_data, h2h_fun = h2h_num_wins,
      players = players,
      absent_players = absent_players, absent_h2h = absent_h2h,
      ...
    )
  }

#' @rdname head-to-head-functions
#' @export
h2h_num_wins <- function(matchup_data, half_for_draw = FALSE, ...) {
  not_identity <- matchup_data$player1 != matchup_data$player2
  score1 <- matchup_data$score1[not_identity]
  score2 <- matchup_data$score2[not_identity]
  sum(score2 > score1) + half_for_draw * 0.5 * sum(score2 == score1)
}

