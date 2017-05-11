context("head-to-head-functions")


# Input data --------------------------------------------------------------
cr_data <- data.frame(
  game = rep(1:2, each = 2),
  player = rep(1:2, times = 2),
  score = 31:34,
  scoreSP = 44:41
)
matchup_data <- get_matchups(cr_data)


# h2h_mean_score_diff -----------------------------------------------------
test_that("h2h_mean_score_diff works", {
  expect_equal(h2h_mean_score_diff(matchup_data), 0)

  matchup_data_1 <- matchup_data[c(2, 6), ]
  expect_equal(h2h_mean_score_diff(matchup_data_1), 1)

  expect_silent(h2h_mean_score_diff(matchup_data_1, extraArg = TRUE))
})


# h2h_mean_score ----------------------------------------------------------
test_that("h2h_mean_score works", {
  expect_equal(h2h_mean_score(matchup_data[c(2, 6), ]), 33)
  expect_equal(h2h_mean_score(matchup_data[c(1, 5), ]), 32)

  expect_silent(h2h_mean_score(matchup_data[c(1, 5), ], extraArg = TRUE))
})


# h2h_sum_score_diff ------------------------------------------------------
test_that("h2h_sum_score_diff works", {
  expect_equal(h2h_sum_score_diff(matchup_data), 0)

  matchup_data_1 <- matchup_data[c(2, 6), ]
  expect_equal(h2h_sum_score_diff(matchup_data_1), 2)

  expect_silent(h2h_sum_score_diff(matchup_data_1, extraArg = TRUE))
})


# h2h_sum_score -----------------------------------------------------------
test_that("h2h_sum_score works", {
  expect_equal(h2h_sum_score(matchup_data), sum(matchup_data$score2))

  matchup_data_1 <- matchup_data[c(2, 6), ]
  expect_equal(h2h_sum_score(matchup_data_1), 66)

  expect_silent(h2h_sum_score(matchup_data_1, extraArg = TRUE))
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

# h2h_num -----------------------------------------------------------------
test_that("h2h_num works", {
  expect_equal(h2h_num(matchup_data), nrow(matchup_data))
  expect_equal(h2h_num(matchup_data[1:3, ]), 3)
})

