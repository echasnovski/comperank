#' Markov method
#'
#' Functions to compute [rating][rating-ranking] and [ranking][rating-ranking]
#' using Markov method.
#'
#' @inheritParams rate_massey
#' @param ... Name-value pairs of Head-to-Head functions (see
#'   [h2h_long()][comperes::h2h_long()]).
#' @param fill A named list that for each Head-to-Head function supplies a
#'   single value to use instead of NA for missing pairs (see
#'   [h2h_long()][comperes::h2h_long()]).
#' @param stoch_modify A single function to modify column-stochastic matrix or a
#'   list of them (see [Stochastic matrix modifiers][stoch-modifiers]).
#' @param weights Weights for different stochastic matrices.
#' @param force_nonneg_h2h Whether to force nonnegative values in Head-to-Head
#'   matrix.
#' @inheritParams rank_massey
#'
#' @details Markov ratings are based on players 'voting' for other players being
#' better. Algorithm is as follows:
#'
#' 1. 'Voting' is done with Head-to-Head values supplied in `...` (see
#' [h2h_mat()][comperes::h2h_mat()] for technical details and section __Design
#' of Head-to-Head values__ for design details). Take special care of
#' Head-to-Head values for self plays (when player1 equals player2). __Note__
#' that Head-to-Head values should be non-negative. Use `force_nonneg_h2h =
#' TRUE` to force that by subtracting minimum Head-to-Head value (in case some
#' Head-to-Head value is strictly negative).
#'
#' 1. Head-to-Head values are transformed into matrix which is normalized to be
#' column-stochastic (sum of every column should be equal to 1) Markov matrix
#' _S_. __Note__ that all missing values are converted into 0. To specify other
#' value use `fill` argument.
#'
#' 1. _S_ is modified with `stoch_modify` to deal with possible problems behind
#' _S_, such as reducibility and rows with all 0.
#'
#' 1. Stationary vector is computed based on _S_ as probability transition
#' matrix of Markov chain process (transition probabilities from state __i__ are
#' elements from column __i__). The result is declared as Markov ratings.
#'
#' Considering common values and structure of stochastic matrices one can
#' naturally combine different 'votings' in one stochastic matrix:
#'
#' 1. [Long format][comperes::h2h_long()] of Head-to-Head values is computed
#' using `...` (which in this case should be several expressions for
#' Head-to-Head functions).
#'
#' 1. Each set of Head-to-Head values is transformed into matrix which is
#' normalized to column-stochastic.
#'
#' 1. Each stochastic matrix is modified with respective modifier which is
#' stored in `stoch_modify` (which can be a list of functions).
#'
#' 1. The resulting stochastic matrix is computed as weighted average of
#' modified stochastic matrices.
#'
#' For Head-to-Head functions in `...` (considered as list) and argument
#' `stoch_modify` general R recycling rule is applied. If `stoch_modify` is a
#' function it is transformed to list with one function.
#'
#' `weights` is recycled to the maximum length of two mentioned recycled
#' elements and then is normalized to sum to 1.
#'
#' Ratings are computed based only on games between players of interest (see
#' Players).
#'
#' @inheritSection keener Design of Head-to-Head values
#'
#' @inheritSection massey Players
#'
#' @return `rate_markov()` returns a [tibble][tibble::tibble] with columns
#' `player` (player identifier) and `rating_markov` (Markov
#' [rating][rating-ranking]). The sum of all ratings should be equal to 1.
#' __Bigger value indicates better player performance__.
#'
#' `rank_markov` returns a `tibble` with columns `player`, `rating_markov` (if
#' `keep_rating = TRUE`) and `ranking_markov` (Markov [ranking][rating-ranking]
#' computed with [round_rank()]).
#'
#' @references \href{https://en.wikipedia.org/wiki/Markov_chain}{Wikipedia
#'   page} for Markov chain.
#'
#' @examples
#' rate_markov(
#'   cr_data = ncaa2005,
#'   # player2 "votes" for player1 if player1 won
#'   comperes::num_wins(score1, score2, half_for_draw = FALSE),
#'   stoch_modify = vote_equal
#' )
#'
#' rank_markov(
#'   cr_data = ncaa2005,
#'   comperes::num_wins(score1, score2, half_for_draw = FALSE),
#'   stoch_modify = vote_equal
#' )
#'
#' rank_markov(
#'   cr_data = ncaa2005,
#'   comperes::num_wins(score1, score2, half_for_draw = FALSE),
#'   stoch_modify = vote_equal,
#'   keep_rating = TRUE
#' )
#'
#' # Combine multiple stochastic matrices and
#' # use inappropriate `fill` which misrepresents reality
#' rate_markov(
#'   cr_data = ncaa2005[-(1:2), ],
#'   win = comperes::num_wins(score1, score2, half_for_draw = FALSE),
#'   # player2 "votes" for player1 proportionally to the amount player1 scored
#'   # more in direct confrontations
#'   score_diff = max(mean(score1 - score2), 0),
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
  assert_h2h_fun(...)

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
  res_vec <- get_pf_vec(stoch)
  names(res_vec) <- rownames(stoch)

  enframe_vec(res_vec, unique_levels(cr$player), "player", "rating_markov")
}

#' @rdname markov
#' @export
rank_markov <- function(cr_data, ..., fill = list(),
                        stoch_modify = teleport(0.15),
                        weights = 1,
                        force_nonneg_h2h = TRUE,
                        keep_rating = FALSE,
                        ties = c("average", "first", "last",
                                 "random", "max", "min"),
                        round_digits = 7) {
  add_ranking(
    rate_markov(
      cr_data = cr_data, ..., fill = fill, stoch_modify = stoch_modify,
      weights = weights, force_nonneg_h2h = force_nonneg_h2h
    ),
    "rating_markov", "ranking_markov",
    keep_rating = keep_rating, type = "desc",
    ties = ties, round_digits = round_digits
  )
}


# Stochastic matrix modifiers ---------------------------------------------
#' Stochastic matrix modifiers
#'
#' Functions for stochastic matrix modifications.
#'
#' @param teleport_prob Probability of 'teleportation'.
#' @param stoch Input stochastic matrix.
#'
#' @details Modification logic behind `teleport()` assumes that at each step
#' of Markov chain (described by column-stochastic matrix) the decision is made
#' whether to change state according to stochastic matrix or to 'teleport' to
#' any state with equal probability. Probability of 'teleport' is
#' `teleport_prob`. This modification is useful because it ensures
#' irreducibility of stochastic matrix (with `teleport_prob` in (0; 1)).
#' __Note__ that in order to obtain modifier one should call function
#' `teleport()` with some parameter.
#'
#' `vote_equal()` and `vote_self()` modify columns with elements only equal to
#' 0. The former fills them with `1/nrow(stoch)` and the latter changes only the
#' respective diagonal element to 1. This is equivalent to jump to any state
#' with equal probability and to stay in the current state respectively.
#'
#' @return `teleport()` returns a modifier function.
#'
#' `vote_equal()` and `vote_self()` are modifier functions and return modified
#' version of input stochastic matrix.
#'
#' @examples
#' input_stoch <- matrix(c(0, 0.3,
#'                         0, 0.7),
#'                       ncol = 2, byrow = TRUE)
#' teleport(0.15)(input_stoch)
#'
#' vote_equal(input_stoch)
#'
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
                   teleport_prob / nrow(stoch))
  }
}

#' @rdname stoch-modifiers
#' @export
vote_equal <- function(stoch) {
  assert_square_mat(stoch)

  is_zero_col <- dplyr::near(colSums(stoch, na.rm = TRUE), 0)
  stoch[, is_zero_col] <- 1 / nrow(stoch)

  stoch
}

#' @rdname stoch-modifiers
#' @export
vote_self <- function(stoch) {
  assert_square_mat(stoch)

  is_zero_col <- dplyr::near(colSums(stoch, na.rm = TRUE), 0)
  diag(stoch)[is_zero_col] <- 1

  stoch
}

to_stoch_mat <- function(mat) {
  mat[, ] <- mat[, ] / rep(colSums(mat, na.rm = TRUE), each = nrow(mat))
  mat[, ] <- dplyr::if_else(is.na(c(mat)), 0, c(mat))

  mat
}

assert_square_mat <- function(mat) {
  if ((!is.matrix(mat)) || (nrow(mat) != ncol(mat))) {
    stop("Input should be a square matrix.", call. = FALSE)
  }

  mat
}
