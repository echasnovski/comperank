#' Keener rating
#'
#' Function to compute rating using Keener method.
#'
#' @param cr_data Competition results in format ready for
#'   \code{\link[=results-longcr]{to_longcr}}.
#' @param h2h_fun Head-to-Head function to compute Head-to-Head matrix.
#' @param players Vector of players for which rating is computed.
#' @param force_nonneg_h2h Whether to force nonnegative values in Head-to-Head
#'   matrix.
#' @param skew_fun Skew function.
#' @param normalize_fun Normalization function.
#' @param eps Coefficient for forcing irreducibility.
#' @param ... Additional arguments to be passed to methods.
#' @param x Argument for \code{skew_keener}.
#' @param h2h_mat Argument for \code{normalize_keener}.
#'
#' @details Keener rating method is based on Head-to-Head matrix of the
#'   competition results. Therefore it can be used for competitions with
#'   variable number of players. Its algorithm is as follows:
#' \enumerate{
#'   \item Compute Head-to-Head matrix of competition results for \code{players}
#'     with \code{h2h_fun} via \code{\link{get_h2h}}. Ensure that there are no
#'     \code{NA}s. Absent in \code{cr_data} players are dropped. If
#'     \code{force_nonneg_h2h} is \code{TRUE} then the minimum value is
#'     subtracted (in case some Head-to-Head value is strictly negative).
#'     \bold{Note} that Keener method is designed for Head-to-Head matrix with
#'     the following property: the more value in row \bold{i} and column
#'     \bold{j} the better player from row \bold{i} performed than player from
#'     column \bold{j}. Extra argument \code{transpose} for
#'     \code{\link{get_h2h}} can be used to ensure that;
#'   \item Update raw Head-to-Head values (denoted as S) with the
#'     pair-normalization: a_{ij} = (S_ij + 1) / (S_ij + S_ji + 2). This step
#'     should make comparing different players more reasonable;
#'   \item Skew Head-to-Head values with applying \code{skew_fun} to them.
#'     \code{skew_fun} should take numeric vector as first argument and
#'     \code{...} as second. It should return skewed vector. The default skew
#'     function is \code{skew_keener}. This step should make abnormal results
#'     not very abnormal. To omit this step use \code{\link{skip_action}};
#'   \item Normalize Head-to-Head values with \code{normalize_fun} using
#'     \code{cr_data}. \code{normalize_fun} should take Head-to-Head matrix as
#'     the first argument, \code{cr_data} as second and \code{...} as third. It
#'     should return normalized matrix. The default normalization is
#'     \code{normalize_keener} which divides Head-to-Head value of
#'     'player1'-'player2' matchup by the number of games played by 'player1'.
#'     This step should take into account possibly not equal number of games
#'     played by players. To omit this step use \code{\link{skip_action}};
#'   \item Add small value to Head-to-Head matrix to ensure its irreducibility.
#'     If all values are strictly positive then this step is omitted. In other
#'     case small value is computed as the smallest non-zero Head-to-Head value
#'     multiplied by \code{eps}. This step is done to ensure applicability of
#'     Perron-Frobenius theorem;
#'   \item Compute Perron-Frobenius vector of the resultant matrix, i.e. the
#'     strictly positive real eigenvector (which values sum to 1) for eigenvalue
#'     (which is real) of the maximum absolute value. This vector is Keener
#'     rating vector.
#' }
#'
#' @return Named vector of the Keener rating. The sum of all ratings should be
#'   equal to 1.
#'
#' @references James P. Keener (1993) \emph{The Perron-Frobenius theorem and the
#'   ranking of football teams}. SIAM Review, 35(1):80–93, 1993.
#'
#' @examples
#' # Use transpose = TRUE is correct in this case
#' rate_keener(ncaa2005, h2h_sum_score, transpose = TRUE)
#'
#' # Impact of skewing
#' rate_keener(ncaa2005, h2h_sum_score, transpose = TRUE,
#'             skew_fun = skip_action)
#'
#' # Impact of normalization. Use fill = 0 to ensure no NA after get_h2h
#' rate_keener(ncaa2005[-(1:2), ], h2h_sum_score,
#'             transpose = TRUE, fill = 0)
#' rate_keener(ncaa2005[-(1:2), ], h2h_sum_score,
#'             normalize_fun = skip_action,
#'             transpose = TRUE, fill = 0)
#'
#' @name keener
NULL

#' @rdname keener
#' @export
rate_keener <- function(cr_data, h2h_fun, players = NULL,
                        force_nonneg_h2h = TRUE,
                        skew_fun = skew_keener,
                        normalize_fun = normalize_keener,
                        eps = 0.001,
                        ...) {
  # Compute symmetrical Head-to-Head matrix
  h2h_mat <- cr_data %>%
    to_longcr(...) %>%
    get_h2h(h2h_fun = h2h_fun, players = players,
            absent_players = players_drop,
            ...) %>%
    force_nonneg(force = force_nonneg_h2h)
  h2h_mat <- (h2h_mat + 1) / (h2h_mat + t(h2h_mat) + 2)

  # Skew
  h2h_mat[, ] <- skew_fun(h2h_mat[, ], ...)

  # Normalize
  h2h_mat <- normalize_fun(h2h_mat, cr_data, ...)

  # Force irreducibility
  is_min_h2h_zero <- isTRUE(all.equal(min(h2h_mat), 0))
  min_non_zero_h2h <- min(h2h_mat[h2h_mat > 0])
  h2h_mat <- h2h_mat + eps * as.integer(is_min_h2h_zero) * min_non_zero_h2h

  # Compute Perron-Frobenius vector
  res <- get_pf_vec(h2h_mat)
  names(res) <- rownames(h2h_mat)

  res
}

force_nonneg <- function(x, force = TRUE) {
  if (force) {
    x <- x - min(min(x), 0)
  }

  x
}

#' @rdname keener
#' @export
skew_keener <- function(x, ...) {
  (1 + sign(x - 0.5) * sqrt(abs(2*x - 1))) / 2
}

#' @rdname keener
#' @export
normalize_keener <- function(h2h_mat, cr_data, ...) {
  player_games_df <- cr_data %>%
    to_longcr(repair = TRUE) %>%
    count_(vars = "player") %>%
    filter_(.dots = list(
      ~ player %in% rownames(h2h_mat)
    ))

  player_games <- rep(0, nrow(h2h_mat))
  names(player_games) <- rownames(h2h_mat)
  player_games[player_games_df$player] <- player_games_df$n

  assert_used_objects(
    used = rownames(h2h_mat),
    original = player_games_df$player,
    prefix = "normalize_keener: ",
    object_name = "row names of Head-to-Head matrix",
    data_name = "players of cr_data"
  )

  h2h_mat / player_games
}

get_pf_vec <- function(mat) {
  mat_eigen_data <- eigen(mat)

  max_eigenvalue_ind <- which.max(abs(mat_eigen_data$value))
  fp_val <- mat_eigen_data$value[max_eigenvalue_ind]

  fp_vec <- mat_eigen_data$vectors[, max_eigenvalue_ind]
  fp_vec <- fp_vec * sign(Re(fp_vec[1]))

  is_value_real <- isTRUE(all.equal(Im(fp_val), 0))
  is_value_pos <- Re(fp_val) > 0
  is_all_real <- isTRUE(all.equal(Im(fp_vec), rep(0, length(fp_vec))))
  is_all_pos <- all(Re(fp_vec) > 0)

  if (!(is_value_real && is_value_pos && is_all_real && is_all_pos)) {
    stop("Something is wrong with application of Perron-Frobenius theorem.")
  }
  fp_vec <- Re(fp_vec)

  fp_vec / sum(fp_vec)
}
