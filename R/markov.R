#' Markov method
#'
#' Functions to compute rating and ranking using Markov method.
#'
#' @inheritParams rate_massey
#' @param ... Name-value pairs of Head-to-Head functions (see \link{h2h_long}).
#' @param fill A named list that for each Head-to-Head function supplies a
#'   single value to use instead of NA for missing pairs (see \link{h2h_long}).
#' @param stoch_modify A single function to modify stochastic matrix or a list
#'  of them (see \link[=stoch-modifiers]{Stochastic matrix modifiers}).
#' @param weights Weights for different stochastic matrices.
#' @param force_nonneg_h2h Whether to force nonnegative values in Head-to-Head
#'   matrix.
#' @param ties Value for \code{ties} in \code{\link{round_rank}}.
#' @param round_digits Value for \code{round_digits} in
#'   \code{\link{round_rank}}.
#'
#' @details Markov ratings are based on players 'voting' for other players being
#' better. Algorithm is as follows:
#' \enumerate{
#'   \item 'Voting' is done with \link[=h2h_long]{Head-to-Head} values supplied
#'     in \code{...}: the more Head-to-Head value the more votes gets player2
#'     from player1. Take special care of Head-to-Head values for self plays
#'     (when player1 equals player2). \bold{Note} that Head-to-Head values
#'     should be non-negative. Use \code{force_nonneg_h2h = TRUE} to force that
#'     by subtracting minimum Head-to-Head value (in case some Head-to-Head
#'     value is strictly negative).
#'   \item Head-to-Head matrix is normalized to be stochastic (sum of
#'     rows should be equal to 1) Markov matrix \emph{S}. \bold{Note} that all
#'     missing values are converted into 0. To specify other value use `fill`
#'     argument.
#'   \item \emph{S} is modified with \code{stoch_modify} to deal with possible
#'     problems behind \emph{S}, such as reducibility and rows with all 0.
#'   \item Stationary vector is computed based on \emph{S} as probability
#'     transition matrix of Markov chain process. The result is declared as
#'     Markov ratings.
#' }
#'
#' Considering common values and structure of stochastic matrices one can
#' naturally combine different 'votings' in one stochastic matrix:
#' \enumerate{
#'   \item Long format of Head-to-Head values is computed using \code{...}
#'     (which in this case should be several expressiong for Head-to-Head
#'     functions).
#'   \item Each matrix is normalized to stochastic.
#'   \item Each stochastic matrix is modified with respective modifier which is
#'     stored in \code{stoch_modify} (which can be a list of functions);
#'   \item The resulting stochastic matrix is computed as weighted average of
#'     modified stochastic matrices.
#' }
#'
#' For Head-to-Head functions in \code{...} (considered as list) and argument
#' \code{stoch_modify} general R recycling rule is applied. If
#' \code{stoch_modify} is a function it is transformed to list with one
#' function.
#'
#' \code{weights} is recycled to the maximum length of two mentioned recycled
#' elements and then is normalized to sum to 1.
#'
#' Ratings are computed based only on games between players of interest (see
#' Players).
#'
#' @inheritSection massey Players
#'
#' @return \code{rate_markov} returns a named vector of the Markov rating. The
#' sum of all ratings should be equal to 1.
#'
#' \code{rank_markov} returns a named vector of \link[=rating-ranking]{ranking}
#' using \code{\link{round_rank}}.
#'
#' @references \href{https://en.wikipedia.org/wiki/Markov_chain}{Wikipedia
#'   page} for Markov chain.
#'
#' @examples
#' rate_markov(
#'   cr_data = ncaa2005,
#'   # player1 "votes" for player2 if player2 won
#'   comperes::num_wins(score2, score1, half_for_draw = FALSE),
#'   stoch_modify = vote_equal
#' )
#'
#' rank_markov(
#'   cr_data = ncaa2005,
#'   comperes::num_wins(score2, score1, half_for_draw = FALSE),
#'   stoch_modify = vote_equal
#' )
#'
#' rate_markov(
#'   cr_data = ncaa2005[-(1:2), ],
#'   win = num_wins(score2, score1, half_for_draw = FALSE),
#'   # player1 "votes" for player2 by the amount player2 scored more
#'   # in direct confrontations
#'   score_diff = max(mean(score2 - score1), 0),
#'   fill = list(win = 0.5, score_diff = 10),
#'   stoch_modify = list(vote_equal, teleport(0.15)),
#'   weights = c(0.8, 0.2)
#' )
#'
#' @name markov
NULL

#' @rdname markov
#' @export
rate_markov <- function(cr_data, ..., fill = list(),
                        stoch_modify = teleport(0.15), weights = 1,
                        force_nonneg_h2h = TRUE) {
  cr <- as_longcr(cr_data, repair = TRUE)
  # Assert arguments
  if (!is.vector(weights, mode = "numeric")) {
    stop("Object 'weights' should be a numeric vector.")
  }

  # Prepare h2h_fun and stoch_modify
  h2h_fun_list <- rlang::enexprs(...)
  stoch_modify_list <- to_function_list(stoch_modify, var_name = "stoch_modify")

  # Manual recycling of weights
  max_length <- max(length(h2h_fun_list), length(stoch_modify_list))
  weights <- rep(weights, length.out = max_length)
  weights <- weights / sum(weights)

  # Compute long format of Head-to-Head values
  res_h2h_long <- h2h_long(cr, ..., fill = fill)
  h2h_names <- setdiff(colnames(res_h2h_long), c("player1", "player2"))

  # Construct stochastic matrix
  stoch <- mapply(
    function(cur_h2h_name, cur_stoch_modify, cur_weight) {
      res_h2h_long %>%
        to_h2h_mat(value = cur_h2h_name) %>%
        force_nonneg(force_nonneg_h2h) %>%
        to_stoch_mat() %>%
        cur_stoch_modify() %>%
        "*"(cur_weight)
    },
    h2h_names, stoch_modify_list, weights,
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ) %>%
    Reduce(f = `+`)

  # Calculate resulting rating vector
  res <-  get_pf_vec(t(stoch))
  names(res) <- rownames(stoch)

  res
}

#' @rdname markov
#' @export
rank_markov <- function(cr_data, ..., fill = list(),
                        stoch_modify = teleport(0.15),
                        weights = 1,
                        force_nonneg_h2h = TRUE,
                        ties = c("average", "first", "last",
                                 "random", "max", "min"),
                        round_digits = 7) {
  round_rank(
    rate_markov(
      cr_data = cr_data, ..., fill = fill, stoch_modify = stoch_modify,
      weights = weights, force_nonneg_h2h = force_nonneg_h2h
    ),
    type = "desc", ties = ties, round_digits = round_digits
  )
}


# Stochastic matrix modifiers ---------------------------------------------
#' Stochastic matrix modifiers
#'
#' Functions for stochastic matrix modifications.
#'
#' @param teleport_prob Probability of 'teleporation'.
#' @param stoch Input stochastic matrix.
#'
#' @details Modification logic behind \code{teleport} assumes that at each step
#'   of Markov chain (described by stochastic matrix) the decision is made
#'   whether to change state according to stochastic matrix or to 'teleport' to
#'   any state with equal probability. Probability of 'teleport' is
#'   \code{teleport_prob}. This modification is useful because it ensures
#'   irreducibility of stochastic matrix (with \code{teleport_prob} in (0; 1)).
#'   \bold{Note} that in order to obtain modifier one should call function
#'   \code{teleport} with some parameter.
#'
#'   \code{vote_equal} and \code{vote_self} modify rows with elements only equal
#'   to 0. The former fills those rows with \code{1/ncol(stoch)} and the latter
#'   changes only the respective diagonal element to 1. This is equivalent to
#'   jump to any state with equal probability and to stay in the current state
#'   respectively.
#'
#' @return \code{teleport} returns a modifier function.
#'
#' \code{vote_equal} and \code{vote_self} are modifier functions and return
#' modified version of input stochastic matrix.
#'
#' @examples
#' input_stoch <- matrix(c(  0,   0,
#'                         0.3, 0.7),
#'                       ncol = 2, byrow = TRUE)
#' teleport(0.15)(input_stoch)
#' vote_equal(input_stoch)
#' vote_self(input_stoch)
#'
#' @name stoch-modifiers
NULL

#' @rdname stoch-modifiers
#' @export
teleport <- function(teleport_prob = 0.15) {
  if ((!is.numeric(teleport_prob)) ||
      (teleport_prob < 0) || (teleport_prob > 1)) {
    stop("Teleport probability should be a number between 0 and 1.")
  }

  function(stoch) {
    assert_square_mat(stoch)

    to_stoch_mat((1 - teleport_prob) * stoch +
                   teleport_prob / ncol(stoch))
  }
}

#' @rdname stoch-modifiers
#' @export
vote_equal <- function(stoch) {
  assert_square_mat(stoch)

  is_zero_row <- dplyr::near(rowSums(stoch, na.rm = TRUE), 0)
  if (any(is_zero_row)) {
    stoch[is_zero_row, ] <- 1 / ncol(stoch)
  }

  stoch
}

#' @rdname stoch-modifiers
#' @export
vote_self <- function(stoch) {
  assert_square_mat(stoch)

  is_zero_row <- dplyr::near(rowSums(stoch, na.rm = TRUE), 0)
  if (any(is_zero_row)) {
    zero_row_inds <- which(is_zero_row)
    stoch[cbind(zero_row_inds, zero_row_inds)] <- 1
  }

  stoch
}

to_stoch_mat <- function(mat) {
  mat[, ] <- dplyr::if_else(is.na(c(mat)), 0, c(mat))
  row_sums <- rowSums(mat[, ])
  row_sums <- ifelse(dplyr::near(row_sums, 0),
                     sqrt(.Machine$double.eps),
                     row_sums)
  mat[, ] <- mat[, ] / row_sums

  mat
}

assert_square_mat <- function(mat) {
  if ((!is.matrix(mat)) || (nrow(mat) != ncol(mat))) {
    stop("Input should be a square matrix.")
  }
}
