context("score-summary-functions")


# Input data --------------------------------------------------------------
input <- data.frame(
  game = rep(1:20, each = 2),
  player = rep(1:10, times = 4),
  score = 31:70,
  season = rep(1:2, each = 20)
)

# score_mean_sd ------------------------------------------------------
test_that("score_mean_sd works", {
  input_game <- input[1:2, ]
  output_game <- c(meanScore = 31.5, sdScore = 0.707106781186548)

  expect_equal(score_mean_sd(input_game), output_game)

  expect_silent(score_mean_sd(input_game, extraArg = TRUE))
})

test_that("score_mean_sd adds prefix", {
  output_summary <-
    get_score_summary(
      cr_data = input, item = c("season", "player"),
      score_fun = score_mean_sd, prefix = "seasonPlayer_"
    )

  expect_equal(
    colnames(output_summary),
    c("season", "player", "seasonPlayer_meanScore", "seasonPlayer_sdScore")
  )
})


# score_min_max -----------------------------------------------------------
test_that("score_min_max works", {
  input_game <- input[1:2, ]
  output_game <- c(minScore = 31, maxScore = 32)

  expect_equal(score_min_max(input_game), output_game)

  expect_silent(score_min_max(input_game, extraArg = TRUE))
})

test_that("score_min_max adds prefix", {
  output_summary <-
    get_score_summary(
      cr_data = input, item = c("season", "player"),
      score_fun = score_min_max, prefix = "seasonPlayer_"
    )

  expect_equal(
    colnames(output_summary),
    c("season", "player", "seasonPlayer_minScore", "seasonPlayer_maxScore")
  )
})
