
<!-- README.md is generated from README.Rmd. Please edit that file -->
comperank
=========

[![Build Status](https://travis-ci.org/echasnovski/comperank.svg?branch=master)](https://travis-ci.org/echasnovski/comperank) [![codecov](https://codecov.io/gh/echasnovski/comperank/branch/master/graph/badge.svg)](https://codecov.io/gh/echasnovski/comperank)

`comperank` provides tools for computing ranking and rating based on competition results. It is tightly connected to its data infrastructure package [comperes](https://github.com/echasnovski/comperes). Basic knowledge about creating [valid competition results](https://github.com/echasnovski/comperes#store-and-convert) and [Head-to-Head expressions](https://github.com/echasnovski/comperes#head-to-head) with `comperes` is needed in order to efficiently use `comperank`.

Understanding of **competition** is quite general: it is a set of **games** (abstract event) in which **players** (abstract entity) gain some abstract **scores** (typically numeric). The most natural example is sport results, however not the only one. Product rating can also be considered as a competition between products as "players". Here a "game" is a customer that reviews a set of products by rating them with numerical "score" (stars, points, etc.).

**Rating** is a list (in the ordinary sense) of numerical values, one for each player, or the numerical value itself. Its interpretation depends on rating method: either bigger value indicates better player performance or otherwise.

**Ranking** is a rank-ordered list (in the ordinary sense) of players: rank 1 indicates player with best performance.

`comperank` leverages the [tidyverse](https://www.tidyverse.org/) ecosystem of R packages. Among other things, it means that the main output format is [tibble](http://tibble.tidyverse.org/).

Overview
--------

`comperank` gets inspiration from the book ["Who's \#1"](https://www.amazon.com/Whos-1-Science-Rating-Ranking/dp/069116231X) by Langville and Meyer. It provides functionality for the following rating algorithms:

-   Algorithms with **fixed Head-to-Head structure**:
    -   Simplified Massey method with `rate_massey()` and `rank_massey()`.
    -   Simplified Colley method with `rate_colley()` and `rank_colley()`.
-   Algorithms with **variable Head-to-Head structure**:
    -   Keener method with `rate_keener()` and `rank_keener()`.
    -   Markov method with `rate_markov()` and `rank_markov()`.
    -   Offense-Defense method with `rate_od()` and `rank_od()`.
-   Algorithms with **iterative nature**:
    -   General Iterative ratings with `rate_iterative()`, `rank_iterative()`, and `add_iterative_ratings()`.
    -   Elo ratings with `rate_elo()`, `rank_elo()`, and `add_elo_ratings()`.

As you can see, there are three sets of functions:

-   `rate_*()`. Its output is a tibble with columns `player` (player identifier) and at least one `rating_*` (rating value). Names of rating columns depend on rating method.
-   `rank_*()`. Its default output is similar to previous one, but with `ranking_*` instead of rating columns. It runs `rate_*()` and does ranking with correct direction. One can use option `keep_rating = TRUE` to keep rating columns in the output.
-   `add_*_ratings()`. These functions are present only for algorithms with iterative nature and competition results with games only between two players. They return tibble with row corresponding to a game (see wide format in **Structure of competition results**) and extra columns indicating ratings of players before and after the game.

This README provides examples of basic usage of these functions. To learn more about algorithms behind them, see corresponding help pages.

For this README we will need the following packages:

``` r
library(rlang)

# This also loads comperes package
suppressPackageStartupMessages(library(comperank))
```

Installation
------------

`comperank` is not on CRAN yet. You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("echasnovski/comperank")
```

Structure of competition results
--------------------------------

All functions in `comperank` expect competition results in one of the formats from `comperes` package. That is either **long** or **wide** format.

**Long format** is the most abstract way of presenting competition results. Basically, it is a data frame (or tibble) with columns `game` (game identifier), `player` (player identifier) and `score` where *each row represents the score of particular player in particular game*. One game can consist from **variable** number of players which makes this format more usable. Inside a game all players are treated equally.

Programmatically long format is represented with `longcr` S3 class which should be created with `as_longcr()` function from `comperes`.

For examples we will use `ncaa2005` data set from `comperes` package, which is already of `longcr` class. It is an example competition results of an isolated group of Atlantic Coast Conference teams provided in book **"Who's \#1"**:

``` r
ncaa2005
#> # A longcr object:
#> # A tibble: 20 x 3
#>    game player score
#>   <int> <chr>  <int>
#> 1     1 Duke       7
#> 2     1 Miami     52
#> 3     2 Duke      21
#> 4     2 UNC       24
#> 5     3 Duke       7
#> 6     3 UVA       38
#> # ... with 14 more rows
```

**Wide format** is a more convenient way to store results with **fixed** number of players in a game. *Each row represents scores of all players in particular game*. Data should be organized in pairs of columns "player"-"score". Identifier of a pair should go after respective keyword and consist only from digits. For example: `player1`, `score1`, `player2`, `score2`. Order doesn't matter. Column `game` is optional.

Programmatically wide format is represented with `widecr` S3 class which should be created with `as_widecr()` function from `comperes`:

``` r
comperes::as_widecr(ncaa2005)
#> # A widecr object:
#> # A tibble: 10 x 5
#>    game player1 score1 player2 score2
#>   <int> <chr>    <int> <chr>    <int>
#> 1     1 Duke         7 Miami       52
#> 2     2 Duke        21 UNC         24
#> 3     3 Duke         7 UVA         38
#> 4     4 Duke         0 VT          45
#> 5     5 Miami       34 UNC         16
#> 6     6 Miami       25 UVA         17
#> # ... with 4 more rows
```

**All `comperank` functions expect either a data frame with long format structure, or `longcr` object, or `widecr` object**.

Algorithms with fixed Head-to-Head structure
--------------------------------------------

**Massey** and **Colley** methods were initially designed for competitions where:

-   Games are held only between two players.
-   It is assumed that score is numeric and higher values indicate better player performance in a game.

### Massey method

Idea of Massey method is that difference in ratings should be proportional to score difference in direct confrontations. Bigger value indicates better player competition performance.

``` r
rate_massey(ncaa2005)
#> # A tibble: 5 x 2
#>   player rating_massey
#>   <chr>          <dbl>
#> 1 Duke           -24.8
#> 2 Miami           18.2
#> 3 UNC             -8. 
#> 4 UVA             -3.4
#> 5 VT              18

rank_massey(ncaa2005)
#> # A tibble: 5 x 2
#>   player ranking_massey
#>   <chr>           <dbl>
#> 1 Duke                5
#> 2 Miami               1
#> 3 UNC                 4
#> 4 UVA                 3
#> 5 VT                  2

rank_massey(ncaa2005, keep_rating = TRUE)
#> # A tibble: 5 x 3
#>   player rating_massey ranking_massey
#>   <chr>          <dbl>          <dbl>
#> 1 Duke           -24.8              5
#> 2 Miami           18.2              1
#> 3 UNC             -8.               4
#> 4 UVA             -3.4              3
#> 5 VT              18                2
```

### Colley method

Idea of Colley method is that ratings should be proportional to share of player's won games. Bigger value indicates better player performance.

``` r
rank_colley(ncaa2005, keep_rating = TRUE)
#> # A tibble: 5 x 3
#>   player rating_colley ranking_colley
#>   <chr>          <dbl>          <dbl>
#> 1 Duke           0.214              5
#> 2 Miami          0.786              1
#> 3 UNC            0.5                3
#> 4 UVA            0.357              4
#> 5 VT             0.643              2
```

Algorithms with variable Head-to-Head structure
-----------------------------------------------

All algorithms with variable Head-to-Head structure depend on user supplying custom Head-to-Head expression for computing quality of direct confrontations between all pairs of players of interest.

Computation of Head-to-Head values is done with functionality of `comperes` package. Programmatically it is implemented as summary of players' matchups - mini-"games" in `widecr` format between pair of players. In other words, for every directed pair (order matters) of players (including "pair" of player with oneself):

-   Data frame of matchups is computed in wide format, i.e. with columns `game`, `player1`, `score1`, `player2`, `score2`.
-   This data frame is summarised with Head-to-Head expression supplied in [dplyr](http://dplyr.tidyverse.org/) fashion.

For more robust usage `comperes` provides `h2h_funs` - a list of the most common Head-to-Head [expressions](http://rlang.r-lib.org/reference/quotation.html) which are designed to be used with [rlang](http://rlang.r-lib.org/)'s [unquoting](http://rlang.r-lib.org/reference/quasiquotation.html) mechanism. All `comperank` functions are designed to be used smoothly with it.

Examples of computing Head-to-Head values for more clarity:

``` r
# Examples of h2h_funs elements
names(h2h_funs)
#> [1] "mean_score_diff"     "mean_score_diff_pos" "mean_score"         
#> [4] "sum_score_diff"      "sum_score_diff_pos"  "sum_score"          
#> [7] "num_wins"            "num_wins2"           "num"

h2h_funs[1:3]
#> $mean_score_diff
#> mean(score1 - score2)
#> 
#> $mean_score_diff_pos
#> max(mean(score1 - score2), 0)
#> 
#> $mean_score
#> mean(score1)

# Computing Head-to-Head values with unquoting
comperes::h2h_long(ncaa2005, !!! h2h_funs)
#> # A long format of Head-to-Head values:
#> # A tibble: 25 x 11
#>   player1 player2 mean_score_diff mean_score_diff_pos mean_score
#>   <chr>   <chr>             <dbl>               <dbl>      <dbl>
#> 1 Duke    Duke                  0                   0       8.75
#> 2 Duke    Miami               -45                   0       7   
#> 3 Duke    UNC                  -3                   0      21   
#> 4 Duke    UVA                 -31                   0       7   
#> 5 Duke    VT                  -45                   0       0   
#> 6 Miami   Duke                 45                  45      52   
#>   sum_score_diff sum_score_diff_p… sum_score
#>            <int>             <dbl>     <int>
#> 1              0                 0        35
#> 2            -45                 0         7
#> 3             -3                 0        21
#> 4            -31                 0         7
#> 5            -45                 0         0
#> 6             45                45        52
#> # ... with 19 more rows, and 3 more variables: num_wins <dbl>,
#> #   num_wins2 <dbl>, num <int>

comperes::h2h_mat(ncaa2005, !!! h2h_funs["mean_score"])
#> # A matrix format of Head-to-Head values:
#>        Duke Miami  UNC  UVA   VT
#> Duke   8.75   7.0 21.0  7.0  0.0
#> Miami 52.00  34.5 34.0 25.0 27.0
#> UNC   24.00  16.0 12.5  7.0  3.0
#> UVA   38.00  17.0  5.0 18.5 14.0
#> VT    45.00   7.0 30.0 52.0 33.5

# Computing Head-to-Head values manually
comperes::h2h_mat(ncaa2005, mean(score1))
#> # A matrix format of Head-to-Head values:
#>        Duke Miami  UNC  UVA   VT
#> Duke   8.75   7.0 21.0  7.0  0.0
#> Miami 52.00  34.5 34.0 25.0 27.0
#> UNC   24.00  16.0 12.5  7.0  3.0
#> UVA   38.00  17.0  5.0 18.5 14.0
#> VT    45.00   7.0 30.0 52.0 33.5

# To account for self play use `if-else`
comperes::h2h_mat(ncaa2005, if(player1[1] == player2[1]) 0 else mean(score1))
#> # A matrix format of Head-to-Head values:
#>       Duke Miami UNC UVA VT
#> Duke     0     7  21   7  0
#> Miami   52     0  34  25 27
#> UNC     24    16   0   7  3
#> UVA     38    17   5   0 14
#> VT      45     7  30  52  0
```

All functions for methods with variable Head-to-Head structure are designed with this rule in mind: **the more Head-to-Head value the better player1 performed than player2**.

### Keener method

Keener method is based on the idea of "relative strength" - the strength of the player relative to the strength of the players he/she has played against. This is computed based on provided Head-to-Head values and some flexible algorithmic adjustments to make method more robust. Bigger value indicates better player performance.

``` r
rank_keener(ncaa2005, !!! h2h_funs["mean_score"], keep_rating = TRUE)
#> # A tibble: 5 x 3
#>   player rating_keener ranking_keener
#>   <chr>          <dbl>          <dbl>
#> 1 Duke          0.0671              5
#> 2 Miami         0.351               1
#> 3 UNC           0.158               4
#> 4 UVA           0.161               3
#> 5 VT            0.263               2
```

### Markov method

The main idea of Markov method is that players "vote" for other players' performance. Voting is done with Head-to-Head values and the more value the more "votes" gives player2 to player1. For example, if Head-to-Head value is "number of wins" then player2 "votes" for player1 proportionally to number of times player1 won in a matchup with player2. **Beware** of careful consideration of Head-to-Head values for self plays.

Actual "voting" is done in [Markov chain](https://en.wikipedia.org/wiki/Markov_chain) fashion: Head-to-Head values are organized in stochastic matrix which vector of stationary probabilities is declared to be output ratings. Bigger value indicates better player performance.

As stochastic matrices can be averaged (with weights), this is the only method capable of direct averaging ratings for different Head-to-Head expressions.

``` r
rank_markov(ncaa2005, !!! h2h_funs["num_wins"], keep_rating = TRUE)
#> # A tibble: 5 x 3
#>   player rating_markov ranking_markov
#>   <chr>          <dbl>          <dbl>
#> 1 Duke          0.0991              5
#> 2 Miami         0.407               1
#> 3 UNC           0.154               3
#> 4 UVA           0.120               4
#> 5 VT            0.220               2

rank_markov(
  ncaa2005,
  !!! h2h_funs[c("num_wins", "mean_score_diff_pos")],
  weights = c(0.2, 0.8),
  keep_rating = TRUE
)
#> # A tibble: 5 x 3
#>   player rating_markov ranking_markov
#>   <chr>          <dbl>          <dbl>
#> 1 Duke          0.0994              5
#> 2 Miami         0.408               1
#> 3 UNC           0.115               4
#> 4 UVA           0.120               3
#> 5 VT            0.257               2
```

### Offense-Defense method

The idea of Offense-Defense (OD) method is to account for different abilities of players by combining different ratings:

-   For player which can achieve *high* Head-to-Head value (even against the player with strong defense) it is said that he/she has **strong offense** which results into *high* offensive rating.
-   For player which can force their opponents into achieving *low* Head-to-Head value (even if they have strong offense) it is said that he/she has **strong defense** which results into *low* defensive rating.

Offensive and defensive ratings describe different skills of players. In order to fully rate players, OD ratings are computed: offensive ratings divided by defensive. The more OD rating the better player performance.

``` r
rank_od(
  ncaa2005,
  if (player1[1] == player2[1]) 0 else mean(score1),
  keep_rating = TRUE
)
#> # A tibble: 5 x 7
#>   player rating_off rating_def rating_od ranking_off ranking_def
#>   <chr>       <dbl>      <dbl>     <dbl>       <dbl>       <dbl>
#> 1 Duke         34.0      1.69       20.1           5           5
#> 2 Miami       152.       0.803     189.            1           2
#> 3 UNC          48.7      1.16       41.8           4           4
#> 4 UVA          82.0      0.967      84.8           3           3
#> 5 VT          115.       0.411     280.            2           1
#>   ranking_od
#>        <dbl>
#> 1          5
#> 2          2
#> 3          4
#> 4          3
#> 5          1
```

Algorithms with iterative nature
--------------------------------

Rating methods with iterative nature assume that games occur in some particular order. All players have some initial ratings which are updated after every game in order they appear. Although, it is possible to consider games with more than two players, `comperank` only supports competition results with all games between two players.

### Iterative ratings

Iterative ratings represent the general approach to ratings with iterative nature. It needs custom rating function and initial player ratings to perform iterative ratings computation. Rating function should accept four arguments: `rating1` (scalar rating of the first player before the game), `score1` (his score), `rating2` and `score2` for the data about second player's performance. It should return a numeric vector of length 2 with elements respectively representing ratings of players after the game.

All functions assume that the order in which games were played is identical to order of values in column `game` (if present) or is defined by the row order.

Arguably, the most useful function is `add_iterative_ratings()`, which adds to `widecr` format of competition results information about game ratings before and after the game.

`rate_iterative()` and `rank_iterative()` return ratings after the last game.

``` r
# Adds 1 to winner's rating and subtracts 1 from loser's rating
test_rate_fun <- function(rating1, score1, rating2, score2) {
  c(rating1, rating2) + ((score1 >= score2) * 2 - 1) * c(1, -1)
}
add_iterative_ratings(ncaa2005, test_rate_fun)
#> # A widecr object:
#> # A tibble: 10 x 9
#>    game player1 score1 player2 score2 rating1Before rating2Before
#>   <int> <chr>    <int> <chr>    <int>         <dbl>         <dbl>
#> 1     1 Duke         7 Miami       52             0             0
#> 2     2 Duke        21 UNC         24            -1             0
#> 3     3 Duke         7 UVA         38            -2             0
#> 4     4 Duke         0 VT          45            -3             0
#> 5     5 Miami       34 UNC         16             1             1
#> 6     6 Miami       25 UVA         17             2             1
#>   rating1After rating2After
#>          <dbl>        <dbl>
#> 1           -1            1
#> 2           -2            1
#> 3           -3            1
#> 4           -4            1
#> 5            2            0
#> 6            3            0
#> # ... with 4 more rows

# Revert the order of games
ncaa2005_rev <- ncaa2005
ncaa2005_rev$game <- 11 - ncaa2005_rev$game
add_iterative_ratings(ncaa2005_rev, test_rate_fun)
#> # A widecr object:
#> # A tibble: 10 x 9
#>    game player1 score1 player2 score2 rating1Before rating2Before
#>   <dbl> <chr>    <int> <chr>    <int>         <dbl>         <dbl>
#> 1     1 UVA         14 VT          52             0             0
#> 2     2 UNC          3 VT          30             0             1
#> 3     3 UNC          7 UVA          5            -1            -1
#> 4     4 Miami       27 VT           7             0             2
#> 5     5 Miami       25 UVA         17             1            -2
#> 6     6 Miami       34 UNC         16             2             0
#>   rating1After rating2After
#>          <dbl>        <dbl>
#> 1           -1            1
#> 2           -1            2
#> 3            0           -2
#> 4            1            1
#> 5            2           -3
#> 6            3           -1
#> # ... with 4 more rows

# Rating after the last game
rank_iterative(ncaa2005, test_rate_fun, keep_rating = TRUE)
#> # A tibble: 5 x 3
#>   player rating_iterative ranking_iterative
#>   <chr>             <dbl>             <dbl>
#> 1 Duke                 -4                 5
#> 2 Miami                 4                 1
#> 3 UNC                   0                 3
#> 4 UVA                  -2                 4
#> 5 VT                    2                 2
```

### Elo method

Elo method is, basically, an iterative rating method with fixed [Elo](https://en.wikipedia.org/wiki/Elo_rating_system) rating function. General idea is that rating increase for winner should be the bigger the more is rating difference between players. In other words, win over a better player should lead to more rating increase and win over a considerably weaker player shouldn't affect rating that much.

``` r
add_elo_ratings(ncaa2005)
#> # A widecr object:
#> # A tibble: 10 x 9
#>    game player1 score1 player2 score2 rating1Before rating2Before
#>   <int> <chr>    <int> <chr>    <int>         <dbl>         <dbl>
#> 1     1 Duke         7 Miami       52           0             0  
#> 2     2 Duke        21 UNC         24         -15             0  
#> 3     3 Duke         7 UVA         38         -29.4           0  
#> 4     4 Duke         0 VT          45         -43.1           0  
#> 5     5 Miami       34 UNC         16          15            14.4
#> 6     6 Miami       25 UVA         17          30.0          13.7
#>   rating1After rating2After
#>          <dbl>        <dbl>
#> 1        -15         15    
#> 2        -29.4       14.4  
#> 3        -43.1       13.7  
#> 4        -56.2       13.1  
#> 5         30.0       -0.619
#> 6         44.3       -0.564
#> # ... with 4 more rows
add_elo_ratings(ncaa2005_rev)
#> # A widecr object:
#> # A tibble: 10 x 9
#>    game player1 score1 player2 score2 rating1Before rating2Before
#>   <dbl> <chr>    <int> <chr>    <int>         <dbl>         <dbl>
#> 1     1 UVA         14 VT          52           0           0    
#> 2     2 UNC          3 VT          30           0          15    
#> 3     3 UNC          7 UVA          5         -14.4       -15    
#> 4     4 Miami       27 VT           7           0          29.4  
#> 5     5 Miami       25 UVA         17          16.3       -30.0  
#> 6     6 Miami       34 UNC         16          29.3         0.619
#>   rating1After rating2After
#>          <dbl>        <dbl>
#> 1      -15             15  
#> 2      -14.4           29.4
#> 3        0.619        -30.0
#> 4       16.3           13.1
#> 5       29.3          -43.0
#> 6       43.0          -13.1
#> # ... with 4 more rows

rank_elo(ncaa2005, keep_rating = TRUE)
#> # A tibble: 5 x 3
#>   player rating_elo ranking_elo
#>   <chr>       <dbl>       <dbl>
#> 1 Duke       -56.2            5
#> 2 Miami       57.9            1
#> 3 UNC         -1.26           3
#> 4 UVA        -29.2            4
#> 5 VT          28.8            2
rank_elo(ncaa2005_rev, keep_rating = TRUE)
#> # A tibble: 5 x 3
#>   player rating_elo ranking_elo
#>   <chr>       <dbl>       <dbl>
#> 1 Duke       -56.2            5
#> 2 Miami       54.3            1
#> 3 UNC          1.10           3
#> 4 UVA        -26.8            4
#> 5 VT          27.5            2
```
