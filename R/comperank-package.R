#' comperank: Ranking and Rating Methods for Competition Results
#'
#' `comperank` provides tools for computing ranking and rating based on
#' competition results. It is tightly connected to its data infrastructure
#' package [comperes][comperes::comperes-package]. Basic knowledge about
#' creating valid competition results and Head-to-Head expressions with
#' `comperes` is needed in order to efficiently use `comperank`.
#'
#' `comperank` leverages the [tidyverse](https://www.tidyverse.org/) ecosystem
#' of R packages. Among other things, it means that the main output format is
#' [tibble][tibble::tibble].
#'
#' `comperank` gets inspiration from the book ["Who's
#' #1"](https://www.amazon.com/Whos-1-Science-Rating-Ranking/dp/069116231X) by
#' Langville and Meyer. It provides functionality for the following rating
#' algorithms:
#' - Algorithms with __fixed Head-to-Head structure__:
#'     - Simplified Massey method with [rate_massey()] and [rank_massey()].
#'     - Simplified Colley method with [rate_colley()] and [rank_colley()].
#' - Algorithms with __variable Head-to-Head structure__:
#'     - Keener method with [rate_keener()] and [rank_keener()].
#'     - Markov method with [rate_markov()] and [rank_markov()].
#'     - Offense-Defense method with [rate_od()] and [rank_od()].
#' - Algorithms with __iterative nature__:
#'     - General Iterative ratings with [rate_iterative()], [rank_iterative()],
#'     and [add_iterative_ratings()].
#'     - Elo ratings with [rate_elo()], [rank_elo()], and [add_elo_ratings()].
#'
#' `comperank` also offers data sets describing professional snooker in seasons
#' 2016/2017 and 2017/2018. See [snooker_events], [snooker_players],
#' [snooker_matches].
#'
#' To learn more about `comperank` browse vignettes with browseVignettes(package
#' = "comperank").
#'
#' @import dplyr
#' @import comperes
#' @importFrom rlang ":=" .data !! !!!
#' @useDynLib comperank
#' @importFrom Rcpp sourceCpp
"_PACKAGE"
