context("head-to-head-functions")


# Input data --------------------------------------------------------------
cr_data <- data.frame(
  game = rep(1:2, each = 2),
  player = rep(1:2, times = 2),
  score = 31:34,
  scoreSP = 44:41
)
matchup_data <- get_matchups(cr_data)



# get_h2h_mean_score_diff -------------------------------------------------
test_that("get_h2h_mean_score_diff works", {
  output <- matrix(c(0, -1, 1, 0), nrow = 2,
                   dimnames = list(1:2, 1:2))
  class(output) <- c("h2h", "matrix")

  expect_identical(get_h2h_mean_score_diff(cr_data), output)

  cr_data_1 <- bind_rows(cr_data, cr_data)
  cr_data_1$game[5:8] <- rep(3:4, each = 2)

  expect_equal(get_h2h_mean_score_diff(cr_data_1), output)
})



# h2h_mean_score_diff -----------------------------------------------------
test_that("h2h_mean_score_diff works", {
  expect_equal(h2h_mean_score_diff(matchup_data), 0)

  matchup_data_1 <- matchup_data[c(2, 6), ]
  expect_equal(h2h_mean_score_diff(matchup_data_1), 1)

  expect_silent(h2h_mean_score_diff(matchup_data_1, extraArg = TRUE))
})



# get_h2h_mean_score ------------------------------------------------------
test_that("get_h2h_mean_score works", {
  output <- matrix(c(32, 32, 33, 33), nrow = 2,
                   dimnames = list(1:2, 1:2))
  class(output) <- c("h2h", "matrix")

  expect_identical(get_h2h_mean_score(cr_data), output)
})

# h2h_mean_score ----------------------------------------------------------
test_that("h2h_mean_score works", {
  expect_equal(h2h_mean_score(matchup_data[c(2, 6), ]), 33)
  expect_equal(h2h_mean_score(matchup_data[c(1, 5), ]), 32)

  expect_silent(h2h_mean_score(matchup_data[c(1, 5), ], extraArg = TRUE))
})

# get_h2h_sum_score_diff --------------------------------------------------
test_that("get_h2h_sum_score_diff works", {
  output <- matrix(c(0, -2, 2, 0), nrow = 2,
                   dimnames = list(1:2, 1:2))
  class(output) <- c("h2h", "matrix")

  expect_identical(get_h2h_sum_score_diff(cr_data), output)

  cr_data_1 <- bind_rows(cr_data, cr_data)
  cr_data_1$game[5:8] <- rep(3:4, each = 2)

  expect_equal(get_h2h_sum_score_diff(cr_data_1), 2*output)
})


# h2h_sum_score_diff ------------------------------------------------------
test_that("h2h_sum_score_diff works", {
  expect_equal(h2h_sum_score_diff(matchup_data), 0)

  matchup_data_1 <- matchup_data[c(2, 6), ]
  expect_equal(h2h_sum_score_diff(matchup_data_1), 2)

  expect_silent(h2h_sum_score_diff(matchup_data_1, extraArg = TRUE))
})


# get_h2h_sum_score -------------------------------------------------------
test_that("get_h2h_sum_score works", {
  output <- matrix(c(64, 64, 66, 66), nrow = 2,
                   dimnames = list(1:2, 1:2))
  class(output) <- c("h2h", "matrix")

  expect_identical(get_h2h_sum_score(cr_data), output)

  cr_data_1 <- bind_rows(cr_data, cr_data)
  cr_data_1$game[5:8] <- rep(3:4, each = 2)

  expect_equal(get_h2h_sum_score(cr_data_1), 2*output)
})

# h2h_sum_score -----------------------------------------------------------
test_that("h2h_sum_score works", {
  expect_equal(h2h_sum_score(matchup_data), sum(matchup_data$score2))

  matchup_data_1 <- matchup_data[c(2, 6), ]
  expect_equal(h2h_sum_score(matchup_data_1), 66)

  expect_silent(h2h_sum_score(matchup_data_1, extraArg = TRUE))
})
# get_h2h_num_wins --------------------------------------------------------
test_that("get_h2h_num_wins works", {
  output <- matrix(c(0, 0, 2, 0), nrow = 2,
                   dimnames = list(1:2, 1:2))
  class(output) <- c("h2h", "matrix")

  expect_identical(get_h2h_num_wins(cr_data), output)

  cr_data_1 <- bind_rows(cr_data, cr_data)
  cr_data_1$game[5:8] <- rep(3:4, each = 2)

  expect_equal(get_h2h_num_wins(cr_data_1), 2*output)

  cr_data_1$score[7] <- 34
  output_1 <- output
  output_1[1, 2] <- 3
  expect_equal(get_h2h_num_wins(cr_data_1, half_for_draw = FALSE), output_1)
  output_1[1, 2] <- 3.5
  output_1[2, 1] <- 0.5
  expect_equal(get_h2h_num_wins(cr_data_1, half_for_draw = TRUE), output_1)
})


# h2h_num_wins ------------------------------------------------------------
test_that("h2h_num_wins works", {
  expect_equal(h2h_num_wins(matchup_data[c(1, 5), ],
                            half_for_draw = TRUE),
               0)
  expect_equal(h2h_num_wins(matchup_data[c(1, 5), ],
                            half_for_draw = FALSE),
               0)

  matchup_data_1 <- matchup_data
  matchup_data_1 <- dplyr::bind_rows(
    matchup_data_1,
    dplyr::tbl_df(data.frame(
      game = 3, player1 = 1, score1 = 10, player2 = 2, score2 = 10
    ))
  )

  expect_equal(h2h_num_wins(matchup_data_1[c(2, 6, 9), ],
                            half_for_draw = TRUE),
               2.5)
  expect_equal(h2h_num_wins(matchup_data_1[c(2, 6, 9), ],
                            half_for_draw = FALSE),
               2)

  expect_silent(h2h_num_wins(matchup_data_1[c(2, 6, 9), ], extraArg = TRUE))
})
