context("iterative")


# Input, output and functions ---------------------------------------------
test_rate_fun <- function(rating1, score1, rating2, score2) {
  rating_diff <- max(abs(rating1 - rating2), 1)
  c(rating1, rating2) + rating_diff * ((score1 >= score2) * 2 - 1) * c(1, -1)
}

set.seed(1002)
input <- data.frame(
  game = rep(1:10, each = 2),
  player = rep(1:5, times = 4),
  score = runif(20)
)
output <- as_widecr(as_longcr(input))
output$rating1Before <- c( 0,  0,  0, 1, -1,  0,  0,  0, 4, -4)
output$rating2Before <- c( 0,  0, -1, 1, -1,  2, -2, -2, 2, -2)
output$rating1After <-  c(-1,  1, -1, 2, -2, -2,  2, -2, 2, -2)
output$rating2After <-  c( 1, -1,  0, 0,  0,  4, -4,  0, 4, -4)


# rate_iterative ----------------------------------------------------------
test_that("rate_iterative works", {
  output_ratings <- c(0, 2, 4, -2, -4)
  names(output_ratings) <- as.character(1:5)

  expect_equal(
    rate_iterative(
      cr_data = input, rate_fun = test_rate_fun, initial_ratings = 0
    ),
    output_ratings
  )

  input_1 <- input
  input_1$player[20] <- NA

  output_ratings_1 <- c(0, 2, 4, -3, -2)
  names(output_ratings_1) <- as.character(1:5)

  expect_equal(
    rate_iterative(
      cr_data = input_1, rate_fun = test_rate_fun, initial_ratings = 0
    ),
    output_ratings_1
  )
})

test_that("rate_iterative handles factor `player`", {
  input_1 <- input
  input_1$player <- factor(input_1$player, levels = c(1:3, 6, 4:5, 0))
  output_1 <- c(0, 2, 4, 0, -2, -4, 0)
  names(output_1) <- as.character(c(1:3, 6, 4:5, 0))

  expect_equal(
    rate_iterative(input_1, test_rate_fun, initial_ratings = 0),
    output_1
  )
})


# rank_iterative ----------------------------------------------------------
test_that("rank_iterative works", {
  output_rankings <- c(2.5, 2.5, 1, 4, 5)
  names(output_rankings) <- as.character(1:5)

  expect_equal(
    rank_iterative(
      cr_data = input, rate_fun = test_rate_fun,
      initial_ratings = c('1' = 3, '2' = 0, '3' = 0, '4' = 0, '5' = 0)
    ),
    output_rankings
  )
})

test_that("rank_iterative handles factor `player`", {
  input_1 <- input
  input_1$player <- factor(input_1$player, levels = c(1:3, 6, 4:5, 0))
  output_1 <- c(4, 2, 1, 4, 6, 7, 4)
  names(output_1) <- as.character(c(1:3, 6, 4:5, 0))

  expect_equal(
    rank_iterative(input_1, test_rate_fun, initial_ratings = 0),
    output_1
  )
})

# add_iterative_ratings ---------------------------------------------------
test_that("add_iterative_ratings simply works", {
  expect_is(output, "widecr")
  expect_identical(
    add_iterative_ratings(input, test_rate_fun, initial_ratings = 0),
    output
  )
})

test_that("add_iterative_ratings stops on not pairgames", {
  expect_error(add_iterative_ratings(input[-1, ], test_rate_fun,
                                     initial_ratings = 0),
               "not pairgames")
})

test_that("add_iterative_ratings handles different scalar initial rating", {
  output_1 <- output
  output_1$rating1Before <- output_1$rating1Before + 10
  output_1$rating2Before <- output_1$rating2Before + 10
  output_1$rating1After <- output_1$rating1After + 10
  output_1$rating2After <- output_1$rating2After + 10

  expect_equal(
    add_iterative_ratings(input, test_rate_fun, initial_ratings = 10),
    output_1
  )
})

test_that("add_iterative_ratings handles NA in player1", {
  input_1 <- input
  input_1$player[19] <- NA

  output_1 <- output
  output_1$player1[10] <- NA
  output_1$rating1Before[10] <- output_1$rating2Before[10]
  output_1$rating1After[10] <- -1
  output_1$rating2After[10] <- -3

  expect_equal(add_iterative_ratings(input_1, test_rate_fun,
                                     initial_ratings = 0),
               output_1)
})

test_that("add_iterative_ratings handles NA in player2", {
  input_1 <- input
  input_1$player[20] <- NA

  output_1 <- output
  output_1$player2[10] <- NA
  output_1$rating2Before[10] <- output_1$rating1Before[10]
  output_1$rating1After[10] <- -3
  output_1$rating2After[10] <- -5

  expect_equal(add_iterative_ratings(input_1, test_rate_fun,
                                     initial_ratings = 0),
               output_1)
})

test_that("add_iterative_ratings handles NA in both player1 and player2", {
  input_1 <- input
  input_1$player[19] <- NA
  input_1$player[20] <- NA

  output_1 <- output
  output_1$player1[10] <- NA
  output_1$player2[10] <- NA
  output_1$rating1Before[10] <- 0
  output_1$rating2Before[10] <- 0
  output_1$rating1After[10] <- 0
  output_1$rating2After[10] <- 0

  expect_equal(add_iterative_ratings(input_1, test_rate_fun,
                                     initial_ratings = 0),
               output_1)
})

test_that("add_iterative_ratings handles factor `player`", {
  input_1 <- input
  input_1$player <- factor(input_1$player, levels = c(1:3, 6))
  input_1 <- input_1[input_1$game %in% c(1, 4, 6, 9), ]

  output_1 <- as_widecr(as_longcr(input[c(1, 2, 7, 8, 11, 12, 17, 18), ]))
  output_1$player1 <- factor(output_1$player1, levels = c(1:3, 6))
  output_1$player2 <- factor(output_1$player2, levels = c(1:3, 6))
  output_1$rating1Before <- c( 0,  1, -1,  5)
  output_1$rating2Before <- c( 0,  0,  2, -1)
  output_1$rating1After <-  c(-1,  2, -4,  -1)
  output_1$rating2After <-  c( 1, -1,  5,  5)

  expect_equal(
    add_iterative_ratings(input_1, test_rate_fun, initial_ratings = 0),
    output_1
  )

  input_2 <- input
  input_2$player <- factor(input_2$player, levels = 1:11)
  output_2 <- output
  output_2$player1 <- factor(output_2$player1, levels = 1:11)
  output_2$player2 <- factor(output_2$player2, levels = 1:11)
  expect_equal(
    add_iterative_ratings(input_2, test_rate_fun, initial_ratings = 0),
    output_2
  )
})


# to_players_id -----------------------------------------------------------
test_that("to_players_id works", {
  ref_players <- 10:1
  players <- c(1:10, NA, 10:1, NaN)

  expect_equal(to_players_id(players, ref_players),
               c(10:1, 0, 1:10, 0))
})


# get_cr_initial_ratings --------------------------------------------------
test_that("get_cr_initial_ratings works", {
  expect_equal(get_cr_initial_ratings(1:5, 0), rep(0, 5))
  expect_equal(get_cr_initial_ratings(1:5, -1), rep(-1, 5))

  init_ratings <- 10:1
  names(init_ratings) <- 1:10
  expect_equal(get_cr_initial_ratings(1:10, init_ratings), 10:1)
  expect_equal(get_cr_initial_ratings(10:1, init_ratings), 1:10)
  expect_equal(get_cr_initial_ratings(5:1, init_ratings), 6:10)
})

test_that("get_cr_initial_ratings throws errors", {
  expect_error(get_cr_initial_ratings(1:10, "a"),
               "should.*numeric")

  expect_error(get_cr_initial_ratings(1:10, 1:2),
               "missing.*players")

  init_ratings <- 10:1
  names(init_ratings) <- 1:10

  expect_error(
    get_cr_initial_ratings(1:11, init_ratings),
    "missing.*11"
  )
})

test_that("get_cr_initial_ratings drops NA in 'players'", {
  initial_ratings <- c("a" = 1, "b" = 2)

  expect_equal(get_cr_initial_ratings(c("b", "a", NA), initial_ratings),
               2:1)
})

# get_ratings_after -------------------------------------------------------
test_that("get_ratings_after works", {
  output_ratings_after <- dplyr::tibble(
    player = as.character(rep(1:5, 4)),
    rating = c(-1, 1, 1, -1, -1, 0, 2, 0, -2,  0,
               -2, 4, 2, -4, -2, 0, 2, 4, -2, -4)
  )

  expect_equal(get_ratings_after(output), output_ratings_after)
})

