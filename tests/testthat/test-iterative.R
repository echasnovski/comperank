context("iterative")


# Input, output and functions ---------------------------------------------
test_rate_fun <- function(rating1, score1, rating2, score2) {
  rating_diff <- max(abs(rating1 - rating2), 1)
  c(rating1, rating2) + rating_diff * ((score1 >= score2) * 2 - 1) * c(1, -1)
}

set.seed(1002)
cr_data <- data.frame(
  game = rep(1:10, each = 2),
  player = rep(1:5, times = 4),
  score = runif(20)
)
output_base <- as_widecr(as_longcr(cr_data))
output_base$rating1Before <- c( 0,  0,  0, 1, -1,  0,  0,  0, 4, -4)
output_base$rating2Before <- c( 0,  0, -1, 1, -1,  2, -2, -2, 2, -2)
output_base$rating1After <-  c(-1,  1, -1, 2, -2, -2,  2, -2, 2, -2)
output_base$rating2After <-  c( 1, -1,  0, 0,  0,  4, -4,  0, 4, -4)

init_rat_df <- data.frame(
  player = 1:5,
  rating = 0:4
)


# rate_iterative ----------------------------------------------------------
test_that("rate_iterative works", {
  output_1 <- rate_iterative(
    cr_data = cr_data, rate_fun = test_rate_fun, initial_ratings = 0
  )
  output_ref_1 <- tibble::tibble(
    player = 1:5, rating_iterative = c(0, 2, 4, -2, -4)
  )

  expect_equal_tbls(output_1, output_ref_1)

  input_2 <- cr_data
  input_2$player[20] <- NA
  output_2 <- rate_iterative(
    cr_data = input_2, rate_fun = test_rate_fun, initial_ratings = 0
  )
  output_ref_2 <- tibble::tibble(
    player = 1:5, rating_iterative = c(0, 2, 4, -3, -2)
  )

  expect_equal_tbls(output_2, output_ref_2)
})

test_that("rate_iterative handles factor `player`", {
  input <- cr_data
  input$player <- factor(input$player, levels = c(1:3, 6, 4:5, 0))
  output <- rate_iterative(input, test_rate_fun, initial_ratings = 0)
  output_ref <- tibble::tibble(
    player = factor(c(1:3, 6, 4:5, 0), levels = c(1:3, 6, 4:5, 0)),
    rating_iterative = c(0, 2, 4, 0, -2, -4, 0)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rate_iterative handles data frame `initial_ratings`", {
  output <- rate_iterative(
    cr_data, test_rate_fun, initial_ratings = init_rat_df
  )
  output_ref <- tibble::tibble(
    player = 1:5,
    rating_iterative = c(4, 3, 6, 1, -4)
  )

  expect_equal_tbls(output, output_ref)
})


# rank_iterative ----------------------------------------------------------
test_that("rank_iterative works", {
  init_ratings <- c('1' = 3, '2' = 0, '3' = 0, '4' = 0, '5' = 0)
  output_1 <- rank_iterative(
    cr_data = cr_data, rate_fun = test_rate_fun,
    initial_ratings = init_ratings
  )
  output_ref_1 <- tibble::tibble(
    player = 1:5, ranking_iterative = c(2.5, 2.5, 1, 4, 5)
  )

  expect_equal_tbls(output_1, output_ref_1)

  output_2 <- rank_iterative(
    cr_data = cr_data, rate_fun = test_rate_fun,
    initial_ratings = init_ratings, keep_rating = TRUE
  )
  output_ref_2 <- output_ref_1
  output_ref_2$rating_iterative <- c(0, 0, 9, -2, -4)
  output_ref_2 <- output_ref_2[, c("player", "rating_iterative",
                                   "ranking_iterative")]

  expect_equal_tbls(output_2, output_ref_2)
})

test_that("rank_iterative handles factor `player`", {
  input <- cr_data
  input$player <- factor(input$player, levels = c(1:3, 6, 4:5, 0))
  output <- rank_iterative(input, test_rate_fun, initial_ratings = 0)
  output_ref <- tibble::tibble(
    player = factor(c(1:3, 6, 4:5, 0), levels = c(1:3, 6, 4:5, 0)),
    ranking_iterative = c(4, 2, 1, 4, 6, 7, 4)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rank_iterative handles data frame `initial_ratings`", {
  output <- rank_iterative(
    cr_data, test_rate_fun, initial_ratings = init_rat_df,
    keep_rating = FALSE, type = "desc"
  )
  output_ref <- tibble::tibble(
    player = 1:5,
    ranking_iterative = c(2, 3, 1, 4, 5)
  )

  expect_equal_tbls(output, output_ref)
})


# add_iterative_ratings ---------------------------------------------------
test_that("add_iterative_ratings simply works", {
  output <- add_iterative_ratings(cr_data, test_rate_fun, initial_ratings = 0)

  expect_is(output, "widecr")
  expect_equal_tbls(output, output_base)
})

test_that("add_iterative_ratings stops on not pairgames", {
  expect_error(
    add_iterative_ratings(cr_data[-1, ], test_rate_fun, initial_ratings = 0),
    "not pairgames"
  )
})

test_that("add_iterative_ratings handles different scalar initial rating", {
  output <- add_iterative_ratings(cr_data, test_rate_fun, initial_ratings = 10)

  output_ref <- output_base
  output_ref$rating1Before <- output_ref$rating1Before + 10
  output_ref$rating2Before <- output_ref$rating2Before + 10
  output_ref$rating1After <- output_ref$rating1After + 10
  output_ref$rating2After <- output_ref$rating2After + 10

  expect_equal_tbls(output, output_ref)
})

test_that("add_iterative_ratings handles NA in player1", {
  input <- cr_data
  input$player[19] <- NA

  output <- add_iterative_ratings(input, test_rate_fun, initial_ratings = 0)

  output_ref <- output_base
  output_ref$player1[10] <- NA
  output_ref$rating1Before[10] <- output_ref$rating2Before[10]
  output_ref$rating1After[10] <- -1
  output_ref$rating2After[10] <- -3

  expect_equal_tbls(output, output_ref)
})

test_that("add_iterative_ratings handles NA in player2", {
  input <- cr_data
  input$player[20] <- NA

  output <- add_iterative_ratings(input, test_rate_fun, initial_ratings = 0)

  output_ref <- output_base
  output_ref$player2[10] <- NA
  output_ref$rating2Before[10] <- output_ref$rating1Before[10]
  output_ref$rating1After[10] <- -3
  output_ref$rating2After[10] <- -5

  expect_equal_tbls(output, output_ref)
})

test_that("add_iterative_ratings handles NA in both player1 and player2", {
  input <- cr_data
  input$player[19] <- NA
  input$player[20] <- NA

  output <- add_iterative_ratings(input, test_rate_fun, initial_ratings = 0)

  output_ref <- output
  output_ref$player1[10] <- NA
  output_ref$player2[10] <- NA
  output_ref$rating1Before[10] <- 0
  output_ref$rating2Before[10] <- 0
  output_ref$rating1After[10] <- 0
  output_ref$rating2After[10] <- 0

  expect_equal_tbls(output, output_ref)
})

test_that("add_iterative_ratings handles factor `player`", {
  input_1 <- cr_data
  input_1$player <- factor(input_1$player, levels = c(1:3, 6))
  input_1 <- input_1[input_1$game %in% c(1, 4, 6, 9), ]

  output_1 <- add_iterative_ratings(input_1, test_rate_fun, initial_ratings = 0)

  output_ref_1 <- as_widecr(as_longcr(cr_data[c(1, 2, 7, 8, 11, 12, 17, 18), ]))
  output_ref_1$player1 <- factor(output_ref_1$player1, levels = c(1:3, 6))
  output_ref_1$player2 <- factor(output_ref_1$player2, levels = c(1:3, 6))
  output_ref_1$rating1Before <- c( 0,  1, -1,  5)
  output_ref_1$rating2Before <- c( 0,  0,  2, -1)
  output_ref_1$rating1After <-  c(-1,  2, -4,  -1)
  output_ref_1$rating2After <-  c( 1, -1,  5,  5)

  expect_equal_tbls(output_1, output_ref_1)

  input_2 <- cr_data
  input_2$player <- factor(input_2$player, levels = 1:11)

  output_2 <- add_iterative_ratings(input_2, test_rate_fun, initial_ratings = 0)

  output_ref_2 <- output_base
  output_ref_2$player1 <- factor(output_ref_2$player1, levels = 1:11)
  output_ref_2$player2 <- factor(output_ref_2$player2, levels = 1:11)

  expect_equal_tbls(output_2, output_ref_2)
})

test_that("add_iterative_ratings handles data frame `initial_ratings`", {
  output <- add_iterative_ratings(
    cr_data, test_rate_fun, initial_ratings = init_rat_df
  )
  output_ref <- output_base
  output_ref$rating1Before <- c( 0, 2,  4, 2,  2, 4,  2, 2, 4, -4)
  output_ref$rating2Before <- c( 1, 3, -1, 3, -1, 3, -1, 3, 5,  1)
  output_ref$rating1After <-  c(-1, 3, -1, 3, -1, 3,  5, 1, 3,  1)
  output_ref$rating2After <-  c( 2, 2,  4, 2,  2, 4, -4, 4, 6, -4)


  expect_equal_tbls(output, output_ref)
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

test_that("get_cr_initial_ratings handles data frame `initial_ratings`", {
  expect_equal(get_cr_initial_ratings(1:5, init_rat_df), 0:4)
  expect_equal(get_cr_initial_ratings(c(3, 2, 5), init_rat_df), c(2, 1, 4))
})

test_that("get_cr_initial_ratings throws errors", {
  expect_error(get_cr_initial_ratings(1:10, "a"),
               "should.*numeric.*or.*data.*frame")

  expect_error(get_cr_initial_ratings(1:10, init_rat_df[, 1, drop = FALSE]),
               "should.*2.*column")

  expect_error(get_cr_initial_ratings(1:10, 1:2),
               "missing.*players")

  init_ratings <- 10:1
  names(init_ratings) <- 1:10

  expect_error(
    get_cr_initial_ratings(1:11, init_ratings),
    "missing.*11"
  )
  expect_error(
    get_cr_initial_ratings(0:5, init_rat_df),
    "missing.*0"
  )
})

test_that("get_cr_initial_ratings drops NA in 'players'", {
  initial_ratings <- c("a" = 1, "b" = 2)

  expect_equal(get_cr_initial_ratings(c("b", "a", NA), initial_ratings),
               2:1)
})


# get_ratings_after -------------------------------------------------------
test_that("get_ratings_after works", {
  output <- get_ratings_after(output_base)
  output_ref <- tibble::tibble(
    player = rep(1:5, 4),
    rating = c(-1, 1, 1, -1, -1, 0, 2, 0, -2,  0,
               -2, 4, 2, -4, -2, 0, 2, 4, -2, -4)
  )

  expect_equal_tbls(output, output_ref)
})
