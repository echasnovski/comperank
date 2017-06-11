#' Markov rating
#'
#' Function to compute rating using Markov method.
#'
#' @param cr_data Competition results in format ready for
#'   \code{\link[=results-longcr]{to_longcr}}.
#' @param h2h_fun A single Head-to-Head function or a list of them (see
#'   Details).
#' @param players Vector of players for which rating is computed.
#' @param transpose Logical vector: whether to transpose Head-to-Head matrix for
#'   the respective \code{h2h_fun}.
#' @param stoch_modify A single function to modify stochastic matrix or a list
#'  of them (see \link[=stoch-modifiers]{Stochastic matrix modifiers}).
#' @param weights Weights for different stochastic matrices.
#' @param force_nonneg_h2h Whether to force nonnegative values in Head-to-Head
#'   matrix.
#' @param ... Additional arguments to be passed to methods.
#'
#' @details Markov ratings are based on players 'voting' for other players being
#' better. Algorithm is as follows:
#' \enumerate{
#'   \item 'Voting' is done with \link[=head-to-head]{Head-to-Head} values via
#'     \code{h2h_fun}: the more Head-to-Head value the more votes gets player2
#'     from player1. One can use \code{transpose} to transpose resulting
#'     Head-to-Head matrix and \code{force_nonneg_h2h} to force nonnegative
#'     values. \bold{Note} that Head-to-Head values should be non-negative;
#'   \item Head-to-Head matrix is normalized to be stochastic (sum of
#'     rows should be equal to 1) Markov matrix \emph{S};
#'   \item \emph{S} is modified with \code{stoch_modify} to deal with possible
#'     problems behind \emph{S}, such as reducibility and rows with all 0;
#'   \item Stationary vector is computed based on \emph{S} as probability
#'     transition matrix of Markov chain process. The result is declared as
#'     Markov ratings.
#' }
#'
#' Considering common values and structure of stochastic matrices one can
#' naturally combine different 'votings' in one stochastic matrix:
#' \enumerate{
#'   \item Different Head-to-Head matrices are computed with \code{h2h_fun}
#'     (which in this case should be a list of Head-to-Head functions);
#'   \item Each matrix is normalized to stochastic;
#'   \item Each stochastic matrix is modified with respective modifier which is
#'     stored in \code{stoch_modify} (which can be a list of functions);
#'   \item The resulting stochastic matrix is computed as weighted average of
#'     modified stochastic matrices.
#' }
#'
#' For arguments \code{h2h_fun}, \code{transpose} and \code{stoch_modify}
#' general R recycling rule is applied. If \code{h2h_fun} or \code{stoch_modify}
#' is function it is transformed to list with one function.
#'
#' \code{weights} is recycled to the maximum length of three mentioned recycled
#' arguments and then is normalized to sum to 1.
#'
#' @return A named vector of the Markov ratings. The sum of all ratings should
#'   be equal to 1.
#'
#' @references \href{https://en.wikipedia.org/wiki/Markov_chain}{Wikipedia
#'   page} for Markov chain.
#'
#' @examples
#' rate_markov(
#'   cr_data = ncaa2005,
#'   h2h_fun = h2h_num_wins,
#'   stoch_modify = vote_equal
#' )
#'
#' rate_markov(
#'   cr_data = ncaa2005,
#'   h2h_fun = list(h2h_num_wins, h2h_mean_score_diff_pos),
#'   stoch_modify = teleport(0.15),
#'   weights = c(0.3, 0.7)
#' )
#'
#' @aliases markov
#'
#' @export
rate_markov <- function(cr_data, h2h_fun, players = NULL,
                        transpose = FALSE,
                        stoch_modify = teleport(0.15),
                        weights = 1,
                        force_nonneg_h2h = TRUE,
                        ...) {
  # Assert arguments
  if (!is.vector(transpose, mode = "logical")) {
    stop("Object 'transpose' should be a logical vector.")
  }
  if (!is.vector(weights, mode = "numeric")) {
    stop("Object 'weights' should be a numeric vector.")
  }

  # Prepare h2h_fun and stoch_modify
  h2h_fun <- to_function_list(h2h_fun, var_name = "h2h_fun")
  stoch_modify <- to_function_list(stoch_modify, var_name = "stoch_modify")

  # Manual recycling of weights
  max_length <- max(length(h2h_fun), length(stoch_modify), length(transpose))
  weights <- rep(weights, length.out = max_length)
  weights <- weights / sum(weights)

  # Construct stochastic matrix
  stoch <- mapply(
    function(cur_h2h_fun, cur_transpose, cur_stoch_modify, cur_weight) {
      get_h2h(
        cr_data = cr_data,
        h2h_fun = cur_h2h_fun,
        players = players,
        transpose = cur_transpose,
        ...) %>%
        force_nonneg(force_nonneg_h2h) %>%
        to_stoch_mat() %>%
        cur_stoch_modify() %>%
        "*"(cur_weight)
    },
    h2h_fun, transpose, stoch_modify, weights,
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ) %>%
    Reduce(f = `+`)

  # Calculate resulting rating vector
  res <-  get_pf_vec(t(stoch))
  names(res) <- rownames(stoch)

  res
}

is_function_list <- function(x) {
  all(sapply(x, rlang::is_function))
}

to_function_list <- function(x, var_name = "input") {
  if (rlang::is_function(x)) {
    x <- list(x)
  }

  if (!is_function_list(x)) {
    stop("Object '", var_name, "' should be function or list of functions.")
  }

  x
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

  is_zero_row <- dplyr::near(rowSums(stoch), 0)
  if (any(is_zero_row)) {
    stoch[is_zero_row, ] <- 1 / ncol(stoch)
  }

  stoch
}

#' @rdname stoch-modifiers
#' @export
vote_self <- function(stoch) {
  assert_square_mat(stoch)

  is_zero_row <- dplyr::near(rowSums(stoch), 0)
  if (any(is_zero_row)) {
    zero_row_inds <- which(is_zero_row)
    stoch[cbind(zero_row_inds, zero_row_inds)] <- 1
  }

  stoch
}

to_stoch_mat <- function(mat) {
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
