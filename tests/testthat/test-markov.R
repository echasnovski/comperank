context("markov")

library(comperes)
library(rlang)

# Input data --------------------------------------------------------------
cr_data <- ncaa2005
input_stoch <- matrix(c(0.3, 0.7,
                        0.2, 0.8),
                      ncol = 2, byrow = TRUE)


# rate_markov -------------------------------------------------------------
test_that("rate_markov simply works", {
  output_1 <- rate_markov(
    cr_data = cr_data,
    # player1 "votes" for player2 if player2 won
    win = num_wins(score2, score1, half_for_draw = FALSE),
    fill = list(win = 0), stoch_modify = vote_equal,
    weights = 1, force_nonneg_h2h = FALSE
  )
  output_1$rating_markov <- round(output_1$rating_markov, 3)

  output_ref_1 <- dplyr::tibble(
    player = c("Duke", "Miami", "UNC", "UVA", "VT"),
    rating_markov = c(0.088, 0.438, 0.146, 0.109, 0.219)
  )

  expect_equal_tbls(output_1, output_ref_1)

  output_2 <- rate_markov(
    cr_data = cr_data,
    # player1 "votes" for player2 by the amount player2 scored more in direct
    # confrontations
    score_diff = max(mean(score2 - score1), 0),
    fill = list(score_diff = 0), stoch_modify = vote_equal,
    weights = 1, force_nonneg_h2h = FALSE
  )
  output_2$rating_markov <- round(output_2$rating_markov, 3)

  output_ref_2 <- output_ref_1
  output_ref_2$rating_markov <- c(0.088, 0.442, 0.095, 0.11, 0.265)

  expect_equal_tbls(output_2, output_ref_2)
})

test_that("rate_markov handles factor `player`", {
  fac_levs <- c("Duke", "Miami", "UNC", "UVA", "Extra", "VT")
  input <- cr_data
  input$player <- factor(input$player, levels = fac_levs)

  output <- rate_markov(
    cr_data = input,
    # player1 "votes" for player2 if player2 won
    win = num_wins(score2, score1, half_for_draw = FALSE),
    fill = list(win = 0), stoch_modify = vote_equal,
    weights = 1, force_nonneg_h2h = FALSE
  )
  output$rating_markov <- round(output$rating_markov, 3)

  output_ref <- dplyr::tibble(
    player = factor(fac_levs, levels = fac_levs),
    rating_markov = c(0.081, 0.403, 0.134, 0.101, 0.081, 0.201)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rate_markov handles numeric `player`", {
  input <- cr_data
  input$player <- as.integer(factor(input$player))

  output <- rate_markov(
    cr_data = input,
    # player1 "votes" for player2 if player2 won
    win = num_wins(score2, score1, half_for_draw = FALSE),
    fill = list(win = 0), stoch_modify = vote_equal,
    weights = 1, force_nonneg_h2h = FALSE
  )
  output$rating_markov <- round(output$rating_markov, 3)

  output_ref <- dplyr::tibble(
    player = 1:5,
    rating_markov = c(0.088, 0.438, 0.146, 0.109, 0.219)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rate_markov handles multiple Head-to-Head values", {
  output_1 <- rate_markov(
    cr_data = cr_data,
    win = num_wins(score2, score1, half_for_draw = FALSE),
    score_diff = max(mean(score2 - score1), 0),
    fill = list(win = 0, score_diff = 0), stoch_modify = vote_equal,
    weights = c(0.3, 0.7), force_nonneg_h2h = FALSE
  )
  output_1$rating_markov <- round(output_1$rating_markov, 3)

  output_ref_1 <- dplyr::tibble(
    player = c("Duke", "Miami", "UNC", "UVA", "VT"),
    rating_markov = c(0.088, 0.44, 0.11, 0.11, 0.252)
  )

  expect_equal_tbls(output_1, output_ref_1)

  output_2 <- rate_markov(
    cr_data = ncaa2005,
    win = num_wins(score2, score1, half_for_draw = FALSE),
    score_diff = max(mean(score2 - score1), 0),
    fill = list(win = 0, score_diff = 0),
    stoch_modify = list(vote_equal, teleport(0.15)),
    weights = c(0.8, 0.2), force_nonneg_h2h = FALSE
  )
  output_2$rating_markov <- round(output_2$rating_markov, 3)

  output_ref_2 <- dplyr::tibble(
    player = c("Duke", "Miami", "UNC", "UVA", "VT"),
    rating_markov = c(0.09, 0.432, 0.139, 0.112, 0.228)
  )
})

test_that("rate_markov uses argument `fill`", {
  input <- cr_data[-c(1, 2), ]
  output <- rate_markov(
    cr_data = input,
    win = num_wins(score2, score1, half_for_draw = FALSE),
    score_diff = max(mean(score2 - score1), 0),
    fill = list(win = 0.5, score_diff = 10),
    stoch_modify = list(vote_equal, teleport(0.15)),
    weights = c(0.8, 0.2), force_nonneg_h2h = FALSE
  )
  output$rating_markov <- round(output$rating_markov, 3)

  output_ref <- dplyr::tibble(
    player = c("Duke", "Miami", "UNC", "UVA", "VT"),
    rating_markov = c(0.305, 0.308, 0.103, 0.094, 0.191)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rate_markov handles function and list `stoch_modify`", {
  expect_identical(
    rate_markov(ncaa2005, !!h2h_funs[["num"]], stoch_modify = vote_equal),
    rate_markov(ncaa2005, !!h2h_funs[["num"]], stoch_modify = list(vote_equal))
  )
})

test_that("rate_markov does recycling", {
  h2h_fun_list <- h2h_funs[c("num_wins", "num")]
  weights <- c(1, 1)
  stoch_modify <- list(vote_equal, vote_self)

  expect_identical(
    rate_markov(cr_data = ncaa2005, !!!h2h_fun_list,
                stoch_modify = stoch_modify[1], weights = weights),
    rate_markov(cr_data = ncaa2005, !!!h2h_fun_list,
                stoch_modify = rep(stoch_modify[1], 2), weights = weights)
  )
  expect_identical(
    rate_markov(cr_data = ncaa2005, !!!h2h_fun_list,
                stoch_modify = stoch_modify, weights = weights[1]),
    rate_markov(cr_data = ncaa2005, !!!h2h_fun_list,
                stoch_modify = stoch_modify, weights = rep(weights[1], 1))
  )
})

test_that("rate_markov throws errors", {
  expect_error(
    rate_markov(ncaa2005, h2h_num_wins, weights = "a"),
    "numeric"
  )
  expect_error(
    rate_markov(ncaa2005, h2h_num_wins, stoch_modify = list(vote_equal, "a")),
    "function"
  )
})


# rank_markov -------------------------------------------------------------
test_that("rank_markov works", {
  output <- rank_markov(
    cr_data = cr_data,
    num_wins(score2, score1, half_for_draw = FALSE),
    stoch_modify = vote_equal,
    weights = 1,
    force_nonneg_h2h = FALSE
  )
  output_ref <- dplyr::tibble(
    player = c("Duke", "Miami", "UNC", "UVA", "VT"),
    ranking_markov = c(5, 1, 3, 4, 2)
  )

  expect_equal(output, output_ref)
})

test_that("rank_markov handles factor `player`", {
  fac_levs <- c("Duke", "Miami", "UNC", "UVA", "Extra", "VT")
  input <- cr_data
  input$player <- factor(input$player, levels = fac_levs)

  output <- rank_markov(
    cr_data = input,
    # player1 "votes" for player2 if player2 won
    win = num_wins(score2, score1, half_for_draw = FALSE),
    fill = list(win = 0), stoch_modify = vote_equal,
    weights = 1, force_nonneg_h2h = FALSE
  )
  output_ref <- dplyr::tibble(
    player = factor(fac_levs, levels = fac_levs),
    ranking_markov = c(5.5, 1, 3, 4, 5.5, 2)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rank_markov handles numeric `player`", {
  input <- cr_data
  input$player <- as.integer(factor(input$player))

  output <- rank_markov(
    cr_data = input,
    # player1 "votes" for player2 if player2 won
    win = num_wins(score2, score1, half_for_draw = FALSE),
    fill = list(win = 0), stoch_modify = vote_equal,
    weights = 1, force_nonneg_h2h = FALSE
  )

  output_ref <- dplyr::tibble(
    player = 1:5,
    ranking_markov = c(5, 1, 3, 4, 2)
  )

  expect_equal_tbls(output, output_ref)
})

test_that("rank_markov uses argument `fill`", {
  input <- cr_data[-c(1, 2), ]
  output <- rank_markov(
    cr_data = input,
    win = num_wins(score2, score1, half_for_draw = FALSE),
    score_diff = max(mean(score2 - score1), 0),
    fill = list(win = 0.5, score_diff = 10),
    stoch_modify = list(vote_equal, teleport(0.15)),
    weights = c(0.8, 0.2),
    force_nonneg_h2h = FALSE
  )
  output_ref <- dplyr::tibble(
    player = c("Duke", "Miami", "UNC", "UVA", "VT"),
    ranking_markov = c(2, 1, 4, 5, 3)
  )

  expect_equal(output, output_ref)
})


# teleport ----------------------------------------------------------------
test_that("teleport works", {
  teleport_fun <- teleport(0.5)

  # Works correctly
  output_ref <- matrix(c(0.4, 0.6,
                         0.35, 0.65),
                       ncol = 2, byrow = TRUE)
  expect_equal(teleport_fun(input_stoch), output_ref)

  # Throws errors
  expect_error(teleport_fun(input_stoch[, 1, drop = FALSE]), "square")
  expect_error(teleport_fun(input_stoch[, 1, drop = TRUE]), "matrix")
})

test_that("teleport normilizes rows", {
  teleport_fun_1 <- teleport(0.5)
  input <- input_stoch
  input[1, ] <- 0

  output_ref_1 <- matrix(c(0.5, 0.5,
                           0.35, 0.65),
                         ncol = 2, byrow = TRUE)

  expect_equal(teleport_fun_1(input), output_ref_1)

  teleport_fun_2 <- teleport(0)

  expect_equal(teleport_fun_2(input), input)
})

test_that("teleport throws errors", {
  expect_error(teleport("a"))
  expect_error(teleport(-0.00001))
  expect_error(teleport(1.0001))
})


# vote_equal --------------------------------------------------------------
test_that("vote_equal works", {
  expect_equal(vote_equal(input_stoch), input_stoch)

  input_1 <- input_stoch
  input_1[1, ] <- 0

  output_ref_1 <- input_1
  output_ref_1[1, ] <- 0.5

  expect_equal(vote_equal(input_1), output_ref_1)

  input_2 <- input_1
  input_2[2, ] <- 0
  output_ref_2 <- output_ref_1
  output_ref_2[2, ] <- 0.5

  expect_equal(vote_equal(input_2), output_ref_2)
})


# vote_self ---------------------------------------------------------------
test_that("vote_self works", {
  expect_equal(vote_self(input_stoch), input_stoch)

  input_1 <- input_stoch
  input_1[1, ] <- 0

  output_ref_1 <- input_1
  output_ref_1[1, 1] <- 1

  expect_equal(vote_self(input_1), output_ref_1)

  input_2 <- input_1
  input_2[2, ] <- 0
  output_ref_2 <- output_ref_1
  output_ref_2[2, 1] <- 0
  output_ref_2[2, 2] <- 1

  expect_equal(vote_self(input_2), output_ref_2)
})


# to_stoch_mat ------------------------------------------------------------
test_that("to_stoch_mat works", {
  expect_equal(to_stoch_mat(2 * input_stoch), input_stoch)

  input_1 <- input_stoch
  input_1[1, ] <- 0

  expect_equal(to_stoch_mat(input_1), input_1)
})

test_that("to_stoch_mat replaces NA with 0", {
  input_1 <- input_stoch
  input_1[1, 1] <- NA
  output <- to_stoch_mat(input_1)
  output_ref <- matrix(c(  0,   1,
                         0.2, 0.8),
                       ncol = 2, byrow = TRUE)

  expect_equal(output, output_ref)
})


# assert_square_mat -------------------------------------------------------
test_that("assert_square_mat works", {
  expect_silent(assert_square_mat(input_stoch))
  expect_error(assert_square_mat(input_stoch[, 1, drop = FALSE]), "square")
  expect_error(assert_square_mat(input_stoch[, 1, drop = TRUE]), "matrix")
})
