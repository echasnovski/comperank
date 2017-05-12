context("item-summary-functions")


# Input data --------------------------------------------------------------
input <- data.frame(
  game = rep(1:20, each = 2),
  player = rep(1:10, times = 4),
  score = 31:70,
  season = rep(1:2, each = 20)
)

# summary_mean_sd_score ---------------------------------------------------
test_that("summary_mean_sd_score works", {
  input_game <- input[1:2, ]
  output_game <- c(meanScore = 31.5, sdScore = 0.707106781186548)

  expect_equal(summary_mean_sd_score(input_game), output_game)

  expect_silent(summary_mean_sd_score(input_game, extraArg = TRUE))
})

test_that("summary_mean_sd_score adds prefix", {
  output_summary <-
    get_item_summary(
      cr_data = input, item = c("season", "player"),
      summary_fun = summary_mean_sd_score, prefix = "seasonPlayer_"
    )

  expect_equal(
    colnames(output_summary),
    c("season", "player", "seasonPlayer_meanScore", "seasonPlayer_sdScore")
  )
})


# summary_min_max_score ---------------------------------------------------
test_that("summary_min_max_score works", {
  input_game <- input[1:2, ]
  output_game <- c(minScore = 31, maxScore = 32)

  expect_equal(summary_min_max_score(input_game), output_game)

  expect_silent(summary_min_max_score(input_game, extraArg = TRUE))
})

test_that("summary_min_max_score adds prefix", {
  output_summary <-
    get_item_summary(
      cr_data = input, item = c("season", "player"),
      summary_fun = summary_min_max_score, prefix = "seasonPlayer_"
    )

  expect_equal(
    colnames(output_summary),
    c("season", "player", "seasonPlayer_minScore", "seasonPlayer_maxScore")
  )
})

# summary_sum_score -------------------------------------------------------
test_that("summary_sum_score works", {
  input_game <- input[1:2, ]
  output_game <- c(sumScore = 63)

  expect_equal(summary_sum_score(input_game), output_game)

  expect_silent(summary_sum_score(input_game, extraArg = TRUE))
})

test_that("summary_sum_score adds prefix", {
  output_summary <-
    get_item_summary(
      cr_data = input, item = c("season", "player"),
      summary_fun = summary_sum_score, prefix = "seasonPlayer_"
    )

  expect_equal(
    colnames(output_summary),
    c("season", "player", "seasonPlayer_sumScore")
  )
})
