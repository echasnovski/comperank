context("rate-markov")


# Input data --------------------------------------------------------------
input_stoch <- matrix(c(0.3, 0.7,
                        0.2, 0.8),
                      ncol = 2, byrow = TRUE)


# rate_markov -------------------------------------------------------------
test_that("rate_markov simply works", {
  output_1 <- rate_markov(
    cr_data = ncaa2005,
    h2h_fun = h2h_num_wins,
    players = NULL,
    transpose = FALSE,
    stoch_modify = vote_equal,
    weights = 1,
    force_nonneg_h2h = FALSE
  )
  output_ref_1 <- c(0.088, 0.438, 0.146, 0.109, 0.219)
  names(output_ref_1) <- c("Duke", "Miami", "UNC", "UVA", "VT")

  expect_equal(round(output_1, 3), output_ref_1)

  output_2 <- rate_markov(
    cr_data = ncaa2005,
    h2h_fun = h2h_mean_score_diff_pos,
    players = NULL,
    transpose = FALSE,
    stoch_modify = vote_equal,
    weights = 1,
    force_nonneg_h2h = FALSE
  )
  output_ref_2 <- c(0.088, 0.442, 0.095, 0.11, 0.265)
  names(output_ref_2) <- names(output_ref_1)

  expect_equal(round(output_2, 3), output_ref_2)

  output_3 <- rate_markov(
    cr_data = ncaa2005,
    h2h_fun = list(h2h_num_wins, h2h_mean_score_diff_pos),
    players = NULL,
    transpose = FALSE,
    stoch_modify = vote_equal,
    weights = c(0.3, 0.7),
    force_nonneg_h2h = FALSE
  )
  output_ref_3 <- c(0.088, 0.44, 0.11, 0.11, 0.252)
  names(output_ref_3) <- names(output_ref_1)

  expect_equal(round(output_3, 3), output_ref_3)
})

test_that("rate_markov handles extra arguments for `get_h2h`", {
  output_1 <- rate_markov(
    cr_data = ncaa2005,
    h2h_fun = h2h_num_wins,
    players = NULL,
    transpose = TRUE,
    stoch_modify = vote_equal,
    weights = 1,
    force_nonneg_h2h = FALSE
  )
  output_ref_1 <- c(0.438, 0.088, 0.146, 0.219, 0.109)
  names(output_ref_1) <- c("Duke", "Miami", "UNC", "UVA", "VT")

  expect_equal(round(output_1, 3), output_ref_1)

  output_2 <- rate_markov(
    cr_data = ncaa2005,
    h2h_fun = h2h_num_wins,
    players = NULL,
    transpose = FALSE,
    self_play = 10,
    stoch_modify = teleport(0.15),
    weights = 1,
    force_nonneg_h2h = FALSE
  )
  output_ref_2 <- c(0.076, 0.457, 0.141, 0.1, 0.225)
  names(output_ref_2) <- names(output_ref_1)

  expect_equal(round(output_2, 3), output_ref_2)
})

test_that("rate_markov handles functions and lists as inputs", {
  expect_identical(rate_markov(ncaa2005, h2h_num),
                   rate_markov(ncaa2005, list(h2h_num)))
  expect_identical(rate_markov(ncaa2005, h2h_num,
                               stoch_modify = vote_equal),
                   rate_markov(ncaa2005, h2h_num,
                               stoch_modify = list(vote_equal)))
})

test_that("rate_markov does recycling", {
  h2h_fun <- list(h2h_num_wins, h2h_num)
  transpose <- c(TRUE, FALSE)
  self_play <- list(NULL, 1)
  weights <- c(1, 1)
  stoch_modify <- list(vote_equal, vote_self)

  expect_identical(
    rate_markov(cr_data = ncaa2005, h2h_fun = h2h_fun[1],
                transpose = transpose, self_play = self_play,
                stoch_modify = stoch_modify, weights = weights),
    rate_markov(cr_data = ncaa2005, h2h_fun = rep(h2h_fun[1], 2),
                transpose = transpose, self_play = self_play,
                stoch_modify = stoch_modify, weights = weights)
  )
  expect_identical(
    rate_markov(cr_data = ncaa2005, h2h_fun = h2h_fun,
                transpose = transpose[1], self_play = self_play,
                stoch_modify = stoch_modify, weights = weights),
    rate_markov(cr_data = ncaa2005, h2h_fun = h2h_fun,
                transpose = rep(transpose[1], 2), self_play = self_play,
                stoch_modify = stoch_modify, weights = weights)
  )
  expect_identical(
    rate_markov(cr_data = ncaa2005, h2h_fun = h2h_fun,
                transpose = transpose, self_play = self_play[1],
                stoch_modify = stoch_modify, weights = weights),
    rate_markov(cr_data = ncaa2005, h2h_fun = h2h_fun,
                transpose = transpose, self_play = rep(self_play[1], 2),
                stoch_modify = stoch_modify, weights = weights)
  )
  expect_identical(
    rate_markov(cr_data = ncaa2005, h2h_fun = h2h_fun,
                transpose = transpose, self_play = self_play,
                stoch_modify = stoch_modify[1], weights = weights),
    rate_markov(cr_data = ncaa2005, h2h_fun = h2h_fun,
                transpose = transpose, self_play = self_play,
                stoch_modify = rep(stoch_modify[1], 2), weights = weights)
  )
  expect_identical(
    rate_markov(cr_data = ncaa2005, h2h_fun = h2h_fun,
                transpose = transpose, self_play = self_play,
                stoch_modify = stoch_modify, weights = weights[1]),
    rate_markov(cr_data = ncaa2005, h2h_fun = h2h_fun,
                transpose = transpose, self_play = self_play,
                stoch_modify = stoch_modify, weights = rep(weights[1], 1))
  )
})

test_that("rate_markov throws errors", {
  expect_error(rate_markov(ncaa2005, h2h_num_wins, transpose = "a"),
               "logical")
  expect_error(rate_markov(ncaa2005, h2h_num_wins, weights = "a"),
               "numeric")
  expect_error(rate_markov(ncaa2005, list(h2h_num_wins, "a")),
               "function")
  expect_error(rate_markov(ncaa2005, h2h_num_wins,
                           stoch_modify = list(vote_equal, "a")),
               "function")
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


# assert_square_mat -------------------------------------------------------
test_that("assert_square_mat works", {
  expect_silent(assert_square_mat(input_stoch))
  expect_error(assert_square_mat(input_stoch[, 1, drop = FALSE]), "square")
  expect_error(assert_square_mat(input_stoch[, 1, drop = TRUE]), "matrix")
})
