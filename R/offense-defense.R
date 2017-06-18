#' Offense-Defense rating
#'
#' Function to compute rating using Offense-Defense method.
#'
#' @param cr_data Competition results in format ready for
#'   \code{\link[=results-longcr]{to_longcr}}.
#' @param h2h_fun Head-to-Head function to compute Head-to-Head matrix.
#' @param players Vector of players for which rating is computed.
#' @param force_nonneg_h2h Whether to force nonnegative values in Head-to-Head
#'   matrix.
#' @param eps Coefficient for total support.
#' @param tol Tolerance value for iterative algorithm.
#' @param max_iterations Maximum number of iterations for iterative algorithm.
#' @param ... Additional arguments to be passed to methods.
#'
#' @details Offense-Defense (OD) rating is designed for games in which player's
#'   goal is to make higher score than opponent(s). To describe competition
#'   results Head-to-Head matrix is used (which is computed with \code{h2h_fun}
#'   via \code{\link{get_h2h}}). Element in row \bold{i} and column \bold{j}
#'   should represent the score player from column \bold{j} makes in games with
#'   player from row \bold{i}. For pairs of players without common games
#'   Head-to-Head value is computed to 0 (not \code{NA}). \bold{Note} that
#'   values should be non-negative and non-NA. This can be ensured with setting
#'   \code{force_nonneg_h2h} to \code{TRUE}.
#'
#'   For player which can make \emph{high} score (even against the player with
#'   strong defense) it is said that he/she has \bold{strong offense} which
#'   results into \emph{high} offensive rating. For player which can force their
#'   opponents into making \emph{low} score (even if they have strong offense)
#'   it is said that he/she has \bold{strong defense} which results into
#'   \emph{low} defensive rating.
#'
#'   Offensive and defensive ratings describe different skills of players. In
#'   order to fully rate players OD ratings are computed: offensive ratings
#'   divided be defensive. The more OD rating the better the player.
#'
#'   Algorithm for OD ratings is as follows:
#'   \enumerate{
#'     \item Add small value to Head-to-Head matrix to ensure convergence of the
#'       iterative algorithm in the next step. If all values are strictly
#'       positive then this step is omitted. In other case small value is
#'       computed as the smallest non-zero Head-to-Head value multiplied by
#'       \code{eps};
#'     \item Perform iterative fixed point search with the following recurrent
#'       formula: \code{def_{k+1} = A \%*\% inv(t(A) \%*\% inv(def_{k}))} where
#'       \code{def_{k}} is a vector of defensive ratings at iteration \code{k},
#'       \code{A} is a perturbed Head-to-Head matrix, \code{inv(x) = 1 / x}.
#'       Iterative search stops if at least one of two conditions is met:
#'       \itemize{
#'         \item \code{sum(abs(def_{k+1} / def_{k} - 1)) < tol};
#'         \item Number of iterations exceeds maximum number of iterations
#'           \code{max_iterations}.
#'       }
#'     \item Compute offensive ratings: \code{off = t(A) \%*\% inv(def)};
#'     \item Compute OD ratings: \code{OD = off / def}.
#'   }
#'
#' @return Matrix with rows named by players and the following three columns:
#' \itemize{
#'   \item \code{off} - offensive ratings of players;
#'   \item \code{def} - defensive ratings of players;
#'   \item \code{od} - Offense-Defense ratings of players;
#' }
#'
#' @references Amy N. Langville, Carl D. Meyer (2012) \emph{Who’s #1?: The
#'   science of rating and ranking}.
#'
#' Philip A. Knight (2008) \emph{The Sinkhorn-Knopp algorithm:
#' Convergence and applications.}. SIAM Journal of Matrix Analysis,
#' 30(1):261–275, 2008 (For stopping rule of iterative algorithm).
#'
#' @aliases offense-defense
#'
#' @examples
#' rate_od(ncaa2005, h2h_mean_score, self_play = 0)
#'
#' @export
rate_od <- function(cr_data, h2h_fun, players = NULL,
                    force_nonneg_h2h = TRUE,
                    eps = 1e-3, tol = 1e-4, max_iterations = 100,
                    ...) {
  h2h_mat <- cr_data %>%
    to_longcr(...) %>%
    get_h2h(h2h_fun = h2h_fun, players = players,
            absent_h2h = fill_h2h, fill = 0,
            ...) %>%
    force_nonneg(force = force_nonneg_h2h)

  # Force total support
  is_min_h2h_zero <- isTRUE(all.equal(min(h2h_mat), 0))
  min_non_zero_h2h <- min(h2h_mat[h2h_mat > 0])
  h2h_mat <- h2h_mat + eps * as.integer(is_min_h2h_zero) * min_non_zero_h2h

  # OD iterations
  prev <- rep(1, nrow(h2h_mat))
  cur <- od_def_iteration(h2h_mat, prev)
  for (i in 2:max(2, max_iterations)) {
    if (od_stop_stat(prev, cur) < tol) {
      break
    }
    prev <- cur
    cur <- od_def_iteration(h2h_mat, prev)
  }

  def_ratings <- cur
  off_ratings <- t(h2h_mat) %*% (1 / def_ratings)
  od_ratings <- off_ratings / def_ratings

  res <- cbind(off_ratings, def_ratings, od_ratings)
  colnames(res) <- c("off", "def", "od")

  res
}

od_def_iteration <- function(mat, vec) {
  mat %*% (1 / (t(mat) %*% (1 / vec)))
}

od_stop_stat <- function(prev, cur) {
  sum(abs(cur / prev - 1))
}
