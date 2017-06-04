#include <Rcpp.h>
using namespace Rcpp;

//' Internal function to compute iterative ratings
//'
//' C++ implementation of iterative computations.
//'
//' @param rate_fun Rating function (see Details).
//' @param player1_id Integer vector of player1 identifiers.
//' @param score1 Numeric vector of player1 score in games.
//' @param player2_id Integer vector of player2 identifiers.
//' @param score2 Numeric vector of player2 score in games.
//' @param initial_ratings Numeric vector of initial ratings (see Details).
//'
//' @details \code{rate_fun} - function that takes arguments:
//'   \itemize{
//'     \item rating1 - rating of player1 before game;
//'     \item score1 - numeric score of player1 in the game;
//'     \item rating2 - rating of player2 before game;
//'     \item score2 - numeric score of player2 in the game.
//'   }
//' This function should return numeric vector of length 2: rating of player1
//' and player2 after the game.
//'
//' \code{player1_id}, \code{player2_id} - integer vectors of identifiers of
//' players in a form of increasing numbers (1, 2, ...). Identifier 0 is
//' reserved for 'ghost' player. His/her rating is taken as current rating of
//' opponent. For two 'ghost' players ratings before and after are 0.
//'
//' \code{initial_ratings} - numeric vector of length equal to number of unique
//' players in \code{player1_id} and \code{player2_id}. In
//' \code{initial_ratings[player_id - 1]} (indexing from zero) initial rating of
//' player with identifier 'player_id' is stored.
//'
//' @return A list:
//' \itemize{
//'   \item rating1Before - rating of player 1 before the game;
//'   \item rating2Before - rating of player 2 before the game;
//'   \item rating1After - rating of player 1 after the game;
//'   \item rating2After - rating of player 2 after the game.
//' }
//' @keywords internal
// [[Rcpp::export]]
List compute_iterative_ratings(
    Function rate_fun,
    IntegerVector player1_id,
    NumericVector score1,
    IntegerVector player2_id,
    NumericVector score2,
    NumericVector initial_ratings
  ) {
  int n_matches = player1_id.size();
  NumericVector cur_ratings(initial_ratings),
    match_result(2),
    rating1_before(n_matches), rating2_before(n_matches),
    rating1_after(n_matches), rating2_after(n_matches);

  for (int i = 0; i < n_matches; i++) {
    // Computing ratings before the game
    if (player1_id[i] == 0) {
      if (player2_id[i] == 0) {
        rating1_before[i] = 0;
        rating2_before[i] = 0;
        rating1_after[i] = 0;
        rating2_after[i] = 0;
        continue;
      } else {
        rating1_before[i] = cur_ratings[player2_id[i] - 1];
        rating2_before[i] = cur_ratings[player2_id[i] - 1];
      }
    } else {
      if (player2_id[i] == 0) {
        rating1_before[i] = cur_ratings[player1_id[i] - 1];
        rating2_before[i] = cur_ratings[player1_id[i] - 1];
      } else {
        rating1_before[i] = cur_ratings[player1_id[i] - 1];
        rating2_before[i] = cur_ratings[player2_id[i] - 1];
      }
    }

    // Compute ratings after the game
    NumericVector match_result =
      rate_fun(rating1_before[i], score1[i],
               rating2_before[i], score2[i]);
    rating1_after[i] = match_result[0];
    rating2_after[i] = match_result[1];

    // Assign ratings after the game
    if (player1_id[i] == 0) {
      if (player2_id[i] != 0) {
        cur_ratings[player2_id[i] - 1] = rating2_after[i];
      }
    } else {
      if (player2_id[i] == 0) {
        cur_ratings[player1_id[i] - 1] = rating1_after[i];
      } else {
        cur_ratings[player1_id[i] - 1] = rating1_after[i];
        cur_ratings[player2_id[i] - 1] = rating2_after[i];
      }
    }
  }

  return(List::create(_["rating1Before"] = rating1_before,
                      _["rating2Before"] = rating2_before,
                      _["rating1After"] = rating1_after,
                      _["rating2After"] = rating2_after));
}
